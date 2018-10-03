%%%-------------------------------------------------------------------
%%% @author yisheng
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 卡牌大师
%%% @end
%%% Created : 17. 九月 2018 16:28
%%%-------------------------------------------------------------------
-module(lib_card_master).
-author("Administrator").

-include("common.hrl").
-include("role.hrl").
-include("drop.hrl").
-include("ret_num.hrl").
-include("activity.hrl").
-include("op_type.hrl").
-include("card_master.hrl").
-include("config.hrl").
-include("prot_resolve_record/prot_825.hrl").

-record(card_master_info,{
	has_ref = 0,            %% 已经刷新的次数，用于判断是否还可以刷新
	next_time = 0,          %% 本次登录前的累计登录时间
	score = 0,              %% 积分，跨天刷新
	times = 0,              %% 本轮抽了几次
	refresh_times = 0,      %% 剩余的免费刷新次数
	exchange_list = [],     %% 个人目标领取列表和状态
	card_list = [],         %% 本轮抽出来的九张卡牌
	guard = [],             %% 最小保底列表
	reward_list = [],       %% 翻牌列表，对应9个位置
	last_check = 0,         %%
	login_time = 0          %% 记录本次的在线时间
}).

-export([
	handle_info/2,
	cross_day_reset/1,
	stop_activity/2,
	save/2,
	do/3,
	init/1
]).

do(82501, PS, _Req) ->
	push_info(PS);

do(82503, PS, #cs_card_exchange_get{id = Id}) ->
	case catch exchange(PS, Id) of
		ok -> ok;
		{error, Code} -> ?ERROR_TOC(PS#role_state.id, Code)
	end;
do(82505, PS, #cs_card_draw{id = Id}) ->
	case catch fetch(PS, Id) of
		{ok, RoleState_1} -> {ok, RoleState_1};
		{error, Code} -> ?ERROR_TOC(PS#role_state.id, Code)
	end;
do(82506, PS, _Req) ->
	case catch 	refresh(PS) of
		{ok, RoleState_1} -> {ok, RoleState_1};
		{error, Code} -> ?ERROR_TOC(PS#role_state.id, Code)
	end;
%%发送每日积分榜单
do(82507, PS, _Req) ->
	#role_state{sid = RoleSid} = PS,
	svr_card_master_rank:cast({send_daily, RoleSid});
%%发送每周积分榜单
do(82509, PS, _Req) ->
	#role_state{sid = RoleSid} = PS,
	svr_card_master_rank:cast({send_weekly, RoleSid});
%%发送幸运榜单
do(82511, PS, _Req) ->
	#role_state{sid = RoleSid} = PS,
	svr_card_master_rank:cast({send_lucky, RoleSid});
do(82513, PS, _Req) ->
	push_refresh_info(PS);

do(_Cmd, _PS, _Req) ->
	ignore.

stop_activity(_Activity, _) ->
	erlang:spawn(
		fun() ->
			SleepMS = util:rand(5000,15000),
			timer:sleep(SleepMS),
			?DB:execute("truncate table `role_card_master`")
		end),
	ok.

handle_info({'add_refresh_times', Now}, RoleState) ->
	add_refresh_times(RoleState, Now).

%% 判断是否达到增加刷新次数的条件，每分钟检查
add_refresh_times(RoleState, Now) ->
	case lib_activity:is_activity_ongoing(?ACT_ID_CARD_MASTER) of
		true ->
			#role_state{id = RoleId, last_login_time = LastLoginTime } = RoleState,
			Data = get_role_data(RoleId),
			%% NextTime：本次登录之前累计登录的时间, OnlineTime：这次累计登录的时间
			#card_master_info{next_time = NextTime, has_ref = HasRef ,refresh_times = RefreshTimes} = Data,
			OnlineTime = Now - LastLoginTime,
			%% 总登录时间，判断是否达到配置表的时间
			AccTime_1 = OnlineTime + NextTime,
			#conf_sys_config{value = {ConfRefTime, Times}} = conf_sys_config:get(card_master),
			%% HasRef:增加次数，达到配置表的次数Times后不再增加
			case AccTime_1 - ConfRefTime * (HasRef + 1) > 0 andalso HasRef < Times of
				true  ->
					HasRef_1 = HasRef + 1,
					RefreshTimes_1 = RefreshTimes + 1,
					Data_1 = Data#card_master_info{  login_time = OnlineTime, has_ref = HasRef_1, refresh_times = RefreshTimes_1},
					push_refresh_info(RoleState);
				false ->
					HasRef_1 = HasRef,
					RefreshTimes_1 = RefreshTimes,
					Data_1 = Data#card_master_info{  login_time = OnlineTime, has_ref = HasRef_1, refresh_times = RefreshTimes_1}
			end,

			set_role_data(RoleId, Data_1);
		_ ->
			skip
	end.
init(PS) ->
	case lib_activity:is_activity_ongoing(?ACT_ID_CARD_MASTER) of
		true ->
			#role_state{id = RoleId } = PS,
			Data = get_role_data(RoleId),
			#card_master_info{last_check = LastCheck} = Data,
			case time:is_today(LastCheck) of
				false ->
					Data_1 =  cross_day_reward(PS),
					Data_2 = Data_1#card_master_info{last_check = time:unixtime()};
				_ ->
					Data_2 = Data
			end,
			set_role_data(RoleId, Data_2);
		_ ->
			skip
	end.

%% 跨天积分清0，发奖励
cross_day_reset(PS) ->
	case lib_activity:is_activity_ongoing(?ACT_ID_CARD_MASTER)  of
		true ->
			#role_state{id = RoleID, sid = _RoleSid} = PS,
			Data_1 = cross_day_reward(PS),
			set_role_data(RoleID, Data_1),
			push_info(PS),
			ok;
		_ ->
			skip
	end.

cross_day_reward(PS) ->
	#role_state{id = RoleID, sid = _RoleSid} = PS,
	Data = get_role_data(RoleID),
	#card_master_info{score = Score, exchange_list = ExchangeList} = Data,
	Index = lib_activity:get_index(?ACT_ID_CARD_MASTER),
	%% IndexList = conf_card_master_target:get_index_list(Index),
	IndexList = [ ID || {ID, State} <- ExchangeList,State =:= 0],
	ConfList = lists:map(fun(Id) -> conf_card_master_target:get(Index, Id) end, IndexList),
	DropIdList = [DropId || #{drop_id := DropId, score := NeedScore} <- ConfList, Score >= NeedScore],
	DropIdList =:= [] orelse svr_mail:sys2p(PS#role_state.id, 96003, lib_drop_api:get_drop_goods(DropIdList)),
	Length = length(conf_card_master_target:get_index_list(Index)),
	Data_1 = Data#card_master_info{score = 0, exchange_list = [{ID, 0} || ID <- lists:seq(1, Length)]},
	Data_1.

save(RoleState, _) ->
	case lib_activity:is_activity_ongoing(?ACT_ID_CARD_MASTER) of
		true ->
			#role_state{id = RoleId } = RoleState,
			Data = get_role_data(RoleId),
			#card_master_info{ login_time = OnlineTime, next_time = AccTime_2 } = Data,
			NewNextTime  = AccTime_2 + OnlineTime,
			Data_1 = Data#card_master_info{next_time = NewNextTime},
			save_role_data(RoleId, Data_1);
		_ ->
			ok
	end.

push_refresh_info(RoleState) ->
	Data = get_role_data(RoleState#role_state.id),
	#card_master_info{ login_time = OnlineTime, next_time = NextTime, has_ref = HasRef, refresh_times = RefreshTimes} = Data,
	#conf_sys_config{value = {ConfRefTime, _ConfTimes}} = conf_sys_config:get(card_master),
	RefTime = ConfRefTime * (HasRef + 1) - (NextTime + OnlineTime),
	{ok, Bin} = prot_msg:encode_msg(82514, #sc_card_refresh_info{
		has_ref = HasRef,
		next_time = RefTime,
		refresh_times = RefreshTimes
	}),
	lib_logic_send:send_to_sid(RoleState#role_state.sid, Bin),
	ok.

push_info(RoleState) ->
	case lib_activity:is_activity_ongoing(?ACT_ID_CARD_MASTER) of
		true ->
			#role_state{id = RoleId} = RoleState,
			Data = get_role_data(RoleId),
			#card_master_info{ login_time = _OnlineTime, score = Score, next_time = _NextTime, times = Times, refresh_times = RefreshTimes, exchange_list = ExchangeList,
				card_list = CardList, reward_list = RewardList, has_ref = _HasRef} = Data,
%%			#conf_sys_config{value = {ConfRefTime, _ConfTimes}} = conf_sys_config:get(card_master),
%%			RefTime = ConfRefTime * (HasRef + 1) - (NextTime + OnlineTime),
			{ok, Bin} = prot_msg:encode_msg(82502, #sc_card_info{
%%				has_ref = HasRef,
%%				next_time = RefTime,
				refresh_times = RefreshTimes,
				times = Times,                  %% 本轮的已翻牌次数
				score = Score,
				card_list = CardList,
				reward_list = RewardList,
				exchang_list = ExchangeList
			}),
			lib_logic_send:send_to_sid(RoleState#role_state.sid, Bin),
			svr_card_master_rank:cast({send_lucky, RoleState#role_state.sid}),
			ok;
		_ ->
			ok
	end.

%% 刷新卡牌
refresh(RoleState) ->
	case lib_activity:is_activity_ongoing(?ACT_ID_CARD_MASTER) of
		true ->
			Index = lib_activity:get_index(?ACT_ID_CARD_MASTER),
			#role_state{id = RoleId} = RoleState,
			%% 取出九个物品
			CardDropList = fetch_card_list(),
			Data = get_role_data(RoleId),
			#card_master_info{refresh_times = RefreshTimes, card_list = _CardList, reward_list = _RewardList, score = Score} = Data,
			case RefreshTimes >= 1 of
				true ->
					RefreshTimes_1 = RefreshTimes - 1,
					RoleState_1 = RoleState,
					Score_1 = Score;
				false ->
					RefreshTimes_1 = RefreshTimes,
					#{id := _Id, ref_score := RefScore, fet_score := _FetScore, add_score := _AddScore ,add_score2 := AddScore2} = conf_card_master_score:get(Index, 1),
					svr_card_master_rank:cast({add, RoleId, AddScore2, time:unixtime()}),
					Score_1 =   Score + AddScore2,
					lib_role_currency:is_enough_money(RoleState, RefScore, act_score) orelse throw ({error, ?RC_COMMON_ACT_SCORE_NOT_ENOUGH}),
					RoleState_1 = lib_role_currency:cost_items_notify(RoleState, [{?DROP_ACT_SCORE, RefScore}], ?OPT_ACT_CARD_MASTER)
			end,
			%% reward_list初始化全部置为0
			Data_1 = Data#card_master_info{score = Score_1, times = 0, card_list = CardDropList, refresh_times = RefreshTimes_1, reward_list = [{ID, 0} || ID <- lists:seq(1, 9)] },
			set_role_data(RoleId, Data_1),
			push_info(RoleState),
			svr_card_master_rank:cast({send_lucky, RoleState#role_state.sid}),
			{ok,RoleState_1};
		_ ->
			{ok, RoleState}
	end.

%% 领取个人目标
exchange(RoleState, Id) ->
	case lib_activity:is_activity_ongoing(?ACT_ID_CARD_MASTER) of
		true ->
			Index = lib_activity:get_index(?ACT_ID_CARD_MASTER),
			#{id := _Id, drop_id := DropId, score := NeedScore} = conf_card_master_target:get(Index, Id),
			#role_state{id = RoleId} = RoleState,
			Data = get_role_data(RoleId),
			#card_master_info{score = Score, exchange_list = ExchangeList} = Data,
			{Id, State} = lists:keyfind(Id, 1, ExchangeList),
			State =:= 0 orelse throw ({error, ?RC_INVEST_REWARD_ALERDAY_GOT}),
			Score >= NeedScore orelse throw({error, ?RC_ACTIVITY_ROTARY_NOT_ENOUGH_SCORE}),
%%			lib_drop_api:give_drop_asyn(RoleId, DropId, ?OPT_ACT_CARD_MASTER),
			lib_drop_api:send_drop_by_id(RoleId, DropId, ?OPT_ACT_CARD_MASTER, "card_master"),
			NewExchangeList = lists:keystore(Id, 1, ExchangeList, {Id, 1}),
			Data_1 = Data#card_master_info{exchange_list = NewExchangeList},
			set_role_data(RoleId, Data_1),
			{ok, Bin} = prot_msg:encode_msg(82504, #sc_card_exchange_get{
				exchange_list = NewExchangeList
			}),
			lib_logic_send:send_to_sid(RoleState#role_state.sid, Bin),
			ok;
		_ ->
			ok
	end.

%% 抽取卡牌
fetch(RoleState, Id) ->
	case lib_activity:is_activity_ongoing(?ACT_ID_CARD_MASTER) of
		true ->
			%% 查看配置消耗活动积分，增加个人目标的积分，通知到gen_server的两个榜单。
			#role_state{id = RoleId, nickname = RoleName} = RoleState,
			Data = get_role_data(RoleId),
			#card_master_info{score = Score, times = Times, card_list = CardList, guard = Guard, reward_list = RewardList } = Data,
			{Id, DropId_1} = lists:keyfind(Id, 1, RewardList),
			DropId_1 =:= 0  orelse throw ({error, ?RC_NEUTRAL_BOSS_POS_ALREADY_TURN}),
			NewTimes = Times + 1,
			NewTimes < 6 orelse throw({error, ?RC_ACT_ANNI_NO_TIMES}),
			Index = lib_activity:get_index(?ACT_ID_CARD_MASTER),
			#{id := _Id, ref_score:= _RefScore, fet_score:= FetScore, add_score:= AddScore}=conf_card_master_score:get(Index, NewTimes),
			NewScore = Score + AddScore,
			%% 增加的积分AddScore增加到gen_server
			svr_card_master_rank:cast({add, RoleId, AddScore, time:unixtime()}),
			lib_role_currency:is_enough_money(RoleState, FetScore, act_score) orelse throw ({error, ?RC_COMMON_ACT_SCORE_NOT_ENOUGH}),
			RoleState_1 = lib_role_currency:cost_items_notify(RoleState, [{?DROP_ACT_SCORE, FetScore}], ?OPT_ACT_CARD_MASTER),
			NewGuard = all_add_one_times(Guard),
			MaybeAppearIDList = maybe_appear_goods(Guard),
			%% 在九张牌中未翻出来的中抽出
			MaybeAppearIDList_2 = lists:filter( fun(Key) -> lists:keymember(Key, 1, CardList) end, MaybeAppearIDList),

			case MaybeAppearIDList_2 of
				[] ->
					IdList = conf_card_master:get_index_list(Index),
					MaybeAppearIDList_3 = lists:filter( fun(Key) -> lists:keymember(Key, 1, CardList) end, IdList);
				_ ->
					MaybeAppearIDList_3 = MaybeAppearIDList_2
			end,

			#conf_card_master_drop{id = ConfID, drop_id = DropID, is_rumor = IsRumor, is_repeat = IsRepeat} = util:rand_by_weight([conf_card_master:get(Index, ConfID) || ConfID <- MaybeAppearIDList_3], #conf_card_master_drop.fetch_weight),
			%% lib_drop_api:give_drop_asyn(RoleId, DropID, ?OPT_ACT_CARD_MASTER),
			lib_drop_api:send_drop_by_id(RoleId, DropID, ?OPT_ACT_CARD_MASTER, "card_master"),
			NewRewardList = lists:keystore(Id, 1, RewardList,{Id, DropID}),
			NewNewGuard = lists:keyreplace(ConfID, 1, NewGuard, {ConfID, 0}),
			%% 发送传闻
			DropInfo = lib_drop_api:get_drop_info(DropID),
			[{_GType, [GID, _, _, _GNum]}] = DropInfo,
			IsRumor /= 1 orelse svr_rumor:publish(world, 94701, [RoleState#role_state.nickname, conf_goods_type:get_name(GID)]),
			%% 增加幸运榜单
			IsRepeat /=1 orelse svr_card_master_rank:cast({add_lucky, {RoleName ,DropID}}),
			NewCardList = lists:keydelete(ConfID, 1,CardList),
			Data_1 = Data#card_master_info{score = NewScore, times = NewTimes, card_list = NewCardList, guard = NewNewGuard, reward_list = NewRewardList},
			set_role_data(RoleId, Data_1),
			push_info(RoleState),
			{ok, RoleState_1};
		_ ->
			{ok, RoleState}
	end.

%% 取出九个物品
fetch_card_list()->
	Index = lib_activity:get_index(?ACT_ID_CARD_MASTER),
    %% MaybeAppearIDList = maybe_appear_goods(Guard),
	ConfIndex = lib_activity:get_index(?ACT_ID_CARD_MASTER),
	ConfIndexIDList = conf_card_master:get_index_list(ConfIndex),
	CardList = util:rand_by_weight([conf_card_master:get(Index, ConfID) || ConfID <- ConfIndexIDList], #conf_card_master_drop.refresh_weight, 9),
	CardDropList = [{Id, DropId} || #conf_card_master_drop{id =Id, drop_id = DropId} <- CardList],
	CardDropList.

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


-define(SQL_GET_ROLE_DATA, <<"select score, times, refresh_times, exchange_list, card_list, guard, reward_list, last_check, has_ref, next_time from role_card_master where role_id = ~w">>).
init_role_data(RoleId) ->
	SQL = io_lib:format(?SQL_GET_ROLE_DATA, [RoleId]),
	case ?DB:get_row(SQL) of
		[Score, Times, RefreshTimes, ExchangeListDb, CardListDb, GuardDb, RewardListDb, LastCheck, HasRef, NextTime] ->
			#card_master_info{
				score = Score,
				times = Times,
				refresh_times = RefreshTimes,
				exchange_list = type:convert_db_field(list, ExchangeListDb, []),
				card_list     = type:convert_db_field(list, CardListDb, []),
				guard         = type:convert_db_field(list, GuardDb, []),
				reward_list   = type:convert_db_field(list, RewardListDb, []),
				last_check    = LastCheck,
				has_ref       = HasRef,
				next_time     = NextTime
			};


		[] ->
			Len = conf_card_master_target:get_index_list(1),
			Length = length(Len),
			#card_master_info{
				guard = make_guard(),
				card_list = fetch_card_list(),
				reward_list = [{ID, 0} || ID <- lists:seq(1, 9)],
				exchange_list = [{ID, 0} || ID <- lists:seq(1, Length)]
			}
	end.

-define(SQL_SAVE_ROLE_DATA,
	<<"replace into role_card_master(role_id, score, times, refresh_times, exchange_list, card_list, guard, reward_list, last_check, has_ref, next_time) values
	(~w, ~w, ~w, ~w, '~s','~s' ,'~s', '~s', ~w, ~w, ~w)">>).
save_role_data(RoleId, Data) ->
	#card_master_info{score = Score, times = Times, refresh_times = RefreshTimes, exchange_list = ExchangeList,
		card_list = CardList, guard = Guard, reward_list = RewardList, last_check = LastCheck, has_ref = HasRef, next_time = NextTime } = Data,
	SQL = io_lib:format(?SQL_SAVE_ROLE_DATA,
		[
			RoleId,
			Score,
			Times,
			RefreshTimes,
			type:term_to_bitstring(ExchangeList),
			type:term_to_bitstring(CardList),
			type:term_to_bitstring(Guard),
			type:term_to_bitstring(RewardList),
			LastCheck,
			HasRef,
			NextTime
		]),
	?DB:execute(SQL).

%% 最小保底
%% 生成概率保护
make_guard() ->
	ConfIndex = lib_activity:get_index(?ACT_ID_CARD_MASTER),
	IDList = conf_card_master:get_index_list(ConfIndex),
	lists:map(fun(ID) -> {ID, 0} end, IDList).

%% 所有的物品刷新次数+1
all_add_one_times(Guard) ->
	ConfIndex = lib_activity:get_index(?ACT_ID_CARD_MASTER),
	IDList = conf_card_master:get_index_list(ConfIndex),
	all_add_one_times(IDList, Guard).

all_add_one_times(IDList, Guard) ->
	lists:foldl(
		fun(ID, GuardT) ->
			Conf = lists:keyfind(ID, 1, Guard),
			case Conf of
				false ->
					[{ID,1}|Guard];
				_ ->
					NewConf = setelement(2, Conf, element(2, Conf) + 1),
					lists:keystore(ID, 1, GuardT, NewConf)
			end
		end, Guard, IDList).

%% 可能出现物品索引列表
maybe_appear_goods(Guard) ->
	ConfIndex = lib_activity:get_index(?ACT_ID_CARD_MASTER),
	lists:foldl(
		fun({ID, RefreshTimes}, List) ->
			#conf_card_master_drop{min_times = MinTimes} = conf_card_master:get(ConfIndex, ID),
			if
				RefreshTimes >= MinTimes ->
					[ID | List];
				true ->
					List
			end
		end, [], Guard).
