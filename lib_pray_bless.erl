%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 祈福天降
%%% @end
%%% Created : 11. 九月 2018 17:51
%%%-------------------------------------------------------------------
-module(lib_pray_bless).
-author("yisheng").

%% API
-export([
	do/3,
	start_activity/1,
	stop_activity/2,
	cross_day_reset/1
]).

-include("common.hrl").
-include("role.hrl").
-include("drop.hrl").
-include("ret_num.hrl").
-include("activity.hrl").
-include("op_type.hrl").
-include("prot_resolve_record/prot_824.hrl").

-record(pray_bless_info, {
	bless_times = 0,          %% 祈福的次数
	get_gold = 0,             %% 个人获得的奖金
	remain_times = 2,         %% 剩余次数
	bag_list = []             %% 背包{id,num}列表
}).

do(82401, PS, _Data) ->
	query_info(PS);
do(82403, PS, #cs_pray_bless_do{times = Times}) ->
	do_bless(Times, PS);
do(82405, PS, _Data) ->
	query_bless_bag(PS);
do(82407, PS, #cs_pray_bless_get{goods_id = GoodsId}) ->
	get_bless_reward(PS, GoodsId);
do(82408, PS, _Data) ->
	query_bless_records(PS).

start_activity(_Activity) ->
	ok.

stop_activity(_Activity, _) ->
	SQL = io_lib:format(<<"select role_id, bag_list from role_pray_bless">>, []),
	%% 遍历发奖
	send_bag(?DB:get_all(SQL)),

	svr_global_data:put_value(0, lib_pray_bless_records, []),
	%% 获得最多奖金者
	svr_global_data:put_value(0, lib_pray_bless_gold_name, {[], 0}),
	%% 活动结束奖池不清0
	%%svr_global_data:put_value(0, lib_pray_bless_gold_pool, 0),

	erlang:spawn(
		fun() ->
			SleepMS = util:rand(5000, 15000),
			timer:sleep(SleepMS),
			?DB:execute("truncate table `role_pray_bless`")
		end),
	ok.

send_bag([]) -> ok;
send_bag([[RoleId, BagListDb] | Data]) ->
	BagList = type:convert_db_field(list, BagListDb, []),
	svr_mail:sys2p(RoleId, 92701, [{?DROP_ITEM, BagList}]),
	send_bag(Data).

%% 跨天免费次数变为2
cross_day_reset(PS) ->
	case is_activity_ongoing() of
		true ->
			#role_state{id = RoleID} = PS,
			Data = get_role_data(RoleID),
			Data_1 = Data#pray_bless_info{remain_times = 2},
			save_role_data(RoleID, Data_1),
			set_role_data(RoleID, Data_1),
			query_info(PS),
			ok;
		_ ->
			skip
	end.

query_info(RoleState) ->
	case is_activity_ongoing() of
		true ->
			Data = get_role_data(RoleState#role_state.id),
			push_info(RoleState, Data);
		_ ->
			ok
	end.

push_info(RoleState, Data) ->
	#pray_bless_info{remain_times = RemainTimes} = Data,
	GoldPool = svr_global_data:get_value(0, lib_pray_bless_gold_pool, 0),
	{Name, Gold} = svr_global_data:get_value(0, lib_pray_bless_gold_name, {[], 0}),
	{ok, Bin} = prot_msg:encode_msg(82402, #sc_pray_bless_info{
		times = RemainTimes,
		gold = Gold,
		name = Name,
		gold_pool = GoldPool
	}),
	query_bless_records(RoleState),
	lib_logic_send:send_to_sid(RoleState#role_state.sid, Bin).

query_bless_records(RoleState) ->
	Records = svr_global_data:get_value(0, lib_pray_bless_records, []),
	{ok, Bin} = prot_msg:encode_msg(82409, #sc_pray_bless_records{
		list = Records
	}),
	lib_logic_send:send_to_sid(RoleState#role_state.sid, Bin).

%% 82405
query_bless_bag(RoleState) ->
	case is_activity_ongoing() of
		true ->
			Data = get_role_data(RoleState#role_state.id),
			#pray_bless_info{bag_list = BagList} = Data,
			NewBagList = get_bag_info(BagList, []),
			{ok, Bin} = prot_msg:encode_msg(82406, #sc_pray_bless_bag{
				bag_goods_list = NewBagList
			}),
			lib_logic_send:send_to_sid(RoleState#role_state.sid, Bin);
		_ ->
			ok
	end.

%% 生成类型
get_bag_info([], GenBag) -> GenBag;
get_bag_info([Bag | BagList], GenBag) ->
	{Id, Num} = Bag,
	Type = conf_pray_bless:is_bonus(Id),
	get_bag_info(BagList, [{Type, Id, Num} | GenBag]).

%% 背包领取
get_bless_reward(RoleState, GoodId) ->
	#role_state{id = RoleId} = RoleState,
	case check_get_bless_reward(GoodId, RoleId, RoleState) of
		{error, ErrCode} ->
			?ERROR_TOC(RoleId, ErrCode);
		{GoodInfo, NewBagList} ->
			RoleState_1 = lib_role_currency:add_items_notify(RoleState, [{?DROP_ITEM, GoodInfo}], ?OPT_PRAY_BLESS),
			BagList_2 = get_bag_info(NewBagList, []),
			{ok, Bin} = prot_msg:encode_msg(82406, #sc_pray_bless_bag{
				bag_goods_list = BagList_2
			}),
			lib_logic_send:send_to_sid(RoleState#role_state.sid, Bin),
			Data = get_role_data(RoleId),
			Data_1 = Data#pray_bless_info{bag_list = NewBagList},
			save_role_data(RoleId, Data_1),
			set_role_data(RoleId, Data_1),
			{ok, RoleState_1}
	end.

check_get_bless_reward(GoodId, RoleId, _RoleState) ->
	case is_activity_ongoing() of
		true ->
			#pray_bless_info{bag_list = BagList} = get_role_data(RoleId),
			case lists:keyfind(GoodId, 1, BagList) of
				{GoodId, Num} ->
					{{GoodId, Num}, lists:keydelete(GoodId, 1, BagList)};
				false ->
					{error, ?RC_ACTIVITY_ERROR_ARGS}
			end;
		_ ->
			{error, ?RC_ACTIVITY_NOT_GOING}
	end.

%% 许愿
do_bless(Times, RoleState) ->
	#role_state{id = RoleId, nickname = RoleName} = RoleState,
	case check_do_bless(RoleId, Times, RoleState) of
		{error, ErrCode} ->
			?ERROR_TOC(RoleId, ErrCode);
		{ok, CostActScore, AddBlessScore, NewRemainTimes} ->
			GoldPool = svr_global_data:get_value(0, lib_pray_bless_gold_pool, 0),
			svr_global_data:put_value(0, lib_pray_bless_gold_pool, GoldPool + AddBlessScore),
			Data = get_role_data(RoleId),
			#pray_bless_info{get_gold = GetGold, bless_times = BlessTimes, bag_list = BagList} = Data,
			GoodsList = make_bless_get_goods(Times, BlessTimes),
			RoleState_1 = lib_role_currency:cost_items_notify(RoleState, [{?DROP_ACT_SCORE, CostActScore}], ?OPT_PRAY_BLESS),
			%% 是否获得奖金，获得加上到GetGold上。
			{RoleState_2, GetGold_1} = bonus(RoleState_1, GoodsList, RoleName, GetGold),
			%% 加到背包里的物品排除奖金池
			AddBagList = lists:filter( fun({Id, _num}) -> conf_pray_bless:is_bonus(Id) == 0 end, GoodsList),
			NewBag = lib_role_currency:merge_items(BagList ++ AddBagList),
			Data_1 = Data#pray_bless_info{remain_times = NewRemainTimes, bless_times = BlessTimes + Times, bag_list = NewBag, get_gold = GetGold_1},
			save_role_data(RoleId, Data_1),
			save_big_reward_record(RoleName, GoodsList),
			set_role_data(RoleId, Data_1),
			case Times of
				1 -> IsBigGoodsList = GoodsList, IsBigGoodsList;
				_ ->
					%%遍历GoodsList和NewBag，如果是奖池做处理
					IsBigGoodsList = lists:foldl(
						fun({GTypeId, Num}, List) ->
							case conf_pray_bless:is_big(GTypeId) of
								1 -> [{GTypeId, Num} | List];
								_ -> List
							end
						end
						, [], GoodsList),
					IsBigGoodsList
			end,
			push_bless_do(NewRemainTimes, IsBigGoodsList, RoleState_2),
			%% 推送排行榜
			query_bless_records(RoleState_2),
			%% 推送信息
			push_info(RoleState_2, Data_1),
			{ok, RoleState_2}
	end.

bonus(RoleState, [], _RoleName, GetGold) ->
	{RoleState, GetGold};

bonus(RoleState, [{Id, Num} | GoodsList], RoleName, GetGold) ->
	GoldPool = svr_global_data:get_value(0, lib_pray_bless_gold_pool, []),
	{_Name, Gold} = svr_global_data:get_value(0, lib_pray_bless_gold_name, {[], 0}),
	ConfIndex = get_activity_index(),
	ConfRandL = conf_pray_bless:get_rands(ConfIndex),
	case conf_pray_bless:is_bonus(Id) of
		1 ->
			%% 从配置表里面，奖池里面拿钱出来发钱
			{Id, Num, _TimesLimit, _Weight, _LimitWeight, BonusPer, _, _} = lists:keyfind(Id, 1, ConfRandL),
			GoldReward = trunc(GoldPool * BonusPer / 100),
			NewGoldPool = GoldPool - GoldReward,
			svr_global_data:put_value(0, lib_pray_bless_gold_pool, NewGoldPool),
			RoleState_1 = lib_role_currency:add_items_notify(RoleState, [{?DROP_GOLD, GoldReward}], ?OPT_PRAY_BLESS),
			GetGold_1 = GetGold + GoldReward,
			case GetGold_1 > Gold of
				true ->
					svr_global_data:put_value(0, lib_pray_bless_gold_name, {RoleName, GetGold_1});
				_ -> skip
			end;
		_ ->
			GetGold_1 = GetGold,
			RoleState_1 = RoleState
	end,
	bonus(RoleState_1, GoodsList, RoleName, GetGold_1).


push_bless_do(NewRemainTimes, GoodsList, RoleState) ->
	%% 获取最高记录,奖金池。
	GoldPool = svr_global_data:get_value(0, lib_pray_bless_gold_pool, 0),
	{ok, Bin} = prot_msg:encode_msg(82404, #sc_pray_bless_do{
		times = NewRemainTimes,
		gold_pool = GoldPool,
		list = GoodsList
	}),
	lib_logic_send:send_to_sid(RoleState#role_state.sid, Bin).

check_do_bless(RoleId, Times, RoleState) ->
	case is_activity_ongoing() of
		true ->
			#pray_bless_info{remain_times = RemainTimes} = get_role_data(RoleId),
			case RemainTimes > 0 andalso Times =:= 1 of
				%% 祈福一次，有免费次数不消耗积分
				true ->
					{_, _CostActScore, AddBlessScore} = lists:keyfind(Times, 1, config:get_sys_config(pray_bless_scores)),
					NewRemainTimes = RemainTimes - 1,
					{ok, 0, AddBlessScore, NewRemainTimes};
				false ->
					ConfL = config:get_sys_config(pray_bless_scores),
					case lists:keyfind(Times, 1, ConfL) of
						{_, CostActScore, AddBlessScore} ->
							case lib_role_currency:is_enough_money(RoleState, CostActScore, act_score) of
								true ->
									{ok, CostActScore, AddBlessScore, RemainTimes};
								_ ->
									{error, ?RC_COMMON_ACT_SCORE_NOT_ENOUGH}
							end;
						_ ->
							{error, ?RC_ACTIVITY_ERROR_ARGS}
					end
			end;
		_ ->
			{error, ?RC_ACTIVITY_NOT_GOING}
	end.

make_bless_get_goods(Times, AccTimes) ->
	ConfIndex = get_activity_index(),
	ConfRandL = conf_pray_bless:get_rands(ConfIndex),
	ConfRandL_1 = [{GTypeId, Num, TimesLimit, Weight}
		|| {GTypeId, Num, TimesLimit, Weight, _LimitWeight, _BonusPer, _, _} <- ConfRandL, AccTimes < TimesLimit],
	ConfRandL_2 = [{GTypeId, Num, TimesLimit, LimitWeight}
		|| {GTypeId, Num, TimesLimit, _Weight, LimitWeight, _BonusPer, _, _} <- ConfRandL, AccTimes >= TimesLimit],
	ConfRandL_3 = ConfRandL_1 ++ ConfRandL_2,
	%% 遍历检查是否达到奖池的要求。
	ConfRandL_4 = bonus_limit([], ConfRandL_3, svr_global_data:get_value(0, lib_pray_bless_gold_pool, 0)),
	make_bless_get_goods(Times, ConfRandL_4, []).

make_bless_get_goods(0, _ConfRandL, GoodsList) ->
	lists:reverse(GoodsList);
make_bless_get_goods(AddPos, ConfRandL, GoodsList) when AddPos > 0 ->
	{GTypeId, Num, _, _} = util:rand_by_weight(ConfRandL, 4),
	make_bless_get_goods(AddPos - 1, ConfRandL, [{GTypeId, Num} | GoodsList]).

bonus_limit(ConfRandL_4, [], _GoldPool) ->
	ConfRandL_4;
bonus_limit(ConfRandL_4, [Good | ConfRandL_3], GoldPool) ->
	ConfIndex = get_activity_index(),
	ConfRandL = conf_pray_bless:get_rands(ConfIndex),
	{Id, _Num, _TimesLimit, _Weight} = Good,
	{_, _, _, _, _, _, _, BonusLimit} = lists:keyfind(Id, 1, ConfRandL),
	%% 判断是否符合奖池的限制条件
	ConfRandL_5 = case conf_pray_bless:is_bonus(Id) of
		              1 ->
			              case GoldPool > BonusLimit of
				              true ->
					              [Good | ConfRandL_4];
				              false ->
					              ConfRandL_4
			              end;
		              0 ->
			              [Good | ConfRandL_4]
	              end,
	bonus_limit(ConfRandL_5, ConfRandL_3, GoldPool).

save_big_reward_record(RoleName, GoodsList) ->
	Now = time:unixtime(),
	BigGoodsList = lists:foldl(
		fun({GTypeId, Num}, List) ->

			case conf_pray_bless:is_big(GTypeId) of
				1 ->
					case conf_pray_bless:is_bonus(GTypeId) of
						1 ->
							[{Now, RoleName, 0, Num} | List];
						_ ->
							[{Now, RoleName, GTypeId, Num} | List]
					end;
				_ ->
					List
			end
		end
		, [], GoodsList),
	case BigGoodsList of
		[] -> skip;
		_ ->
			Records = svr_global_data:get_value(0, lib_pray_bless_records, []),
			Records_1 = lists:sublist(BigGoodsList ++ Records, 30),
			svr_global_data:put_value(0, lib_pray_bless_records, Records_1)
	end.

is_activity_ongoing() ->
	lib_activity:is_activity_ongoing(?ACT_ID_PRAY_BLESS).

get_activity_index() ->
	lib_activity:get_index(?ACT_ID_PRAY_BLESS).

get_role_data(RoleId) ->
	case erlang:get({?MODULE, role_data}) of
		undefined ->
			Data = init_role_data(RoleId),
			set_role_data(RoleId, Data),
			Data;
		Data ->
			Data
	end.

set_role_data(_RoleId, Data) ->
	erlang:put({?MODULE, role_data}, Data).

-define(SQL_GET_ROLE_DATA, <<"select bless_times, remain_times, bag_list from role_pray_bless where role_id = ~w">>).
init_role_data(RoleId) ->
	SQL = io_lib:format(?SQL_GET_ROLE_DATA, [RoleId]),
	case ?DB:get_row(SQL) of
		[BlessTimes, RemainTimes, BagListDb] ->
			#pray_bless_info{bless_times = BlessTimes, remain_times = RemainTimes,
				bag_list = type:convert_db_field(list, BagListDb, [])};
		[] ->
			#pray_bless_info{}
	end.

-define(SQL_SAVE_ROLE_DATA, <<"replace into role_pray_bless(role_id, bless_times, remain_times, bag_list) values (~w, ~w, ~w, '~s')">>).
save_role_data(RoleId, Data) ->
	#pray_bless_info{bless_times = BlessTimes, remain_times = RemainTimes, bag_list = BagList} = Data,
	SQL = io_lib:format(?SQL_SAVE_ROLE_DATA, [RoleId, BlessTimes, RemainTimes, type:term_to_bitstring(BagList)]),
	?DB:execute(SQL).
