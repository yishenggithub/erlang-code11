%%%-------------------------------------------------------------------
%%% @author yisheng
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 五行迷阵
%%% @end
%%% Created : 21. 八月 2018 10:51
%%%-------------------------------------------------------------------
-module(lib_maze_five).
-author("yisheng").

-include("common.hrl").
-include("role.hrl").
-include("activity.hrl").
-include("ret_num.hrl").
-include("maze_five.hrl").
-include("op_type.hrl").
-include("config.hrl").
-include("drop.hrl").
-include("prot_resolve_record/prot_821.hrl").

%% API
-export([
	init/1,
	info/1,
	light/2,
	cross_day_reset/1,
	save/2,
	start_activity/1,
	stop_activity/2,
	handle_info/2,
	do/3
]).

do(82101, PS, _Data) ->
	info(PS);
do(82103, PS, #cs_maze_five_light{posi_id = PosiId}) ->
	light(PS, PosiId).


start_activity(_Activity) ->
	ok.

stop_activity(_Activity, _) ->
	erlang:spawn(
		fun() ->
			SleepMS = util:rand(5000, 15000),
			timer:sleep(SleepMS),
			?DB:execute("truncate table `role_maze_five`")
		end),
	ok.

init(PS) ->
	case is_activity_open() of
		true ->
			init_role_data(PS#role_state.id);
		_ ->
			skip
	end.

init_role_data(RoleID) ->
	{Posi, FiveList, BarrierList, EvilList, PassPosi, Step, Record, LastCheck, RefreshTimes, Guard, IsGet} = get_role_db_data(RoleID),
	{NewStep, NewLastCheck, NewRefreshTimes, NewRecord, Updated} = get_new_data(Step, RefreshTimes, LastCheck, Record),
	NewGuard = ?iif(Guard =:= [], make_guard(), Guard),
	RoleData = #{posi => Posi, five_list => FiveList, barrier_list => BarrierList, evil_list => EvilList, guard => NewGuard, is_get => IsGet,
		pass_posi => PassPosi, step => NewStep, record =>NewRecord, last_check => NewLastCheck, refresh_times => NewRefreshTimes, updated => Updated},
	?iif(Updated, update_role_data(RoleID, RoleData), set_role_data(RoleID, RoleData)),
	RoleData.

get_new_data(Step, RefreshTimes, LastCheck, Record) ->
	case time:is_today(LastCheck) of
		false ->
			NewStep = Step + 1,
			NewRefreshTimes = 0,
			NewRecord = 0,
			{NewStep, time:unixtime(), NewRefreshTimes, NewRecord, true};
		_ ->
			{Step, LastCheck, RefreshTimes, Record, false}
	end.

update_role_data(RoleId, Data) ->
	NewData = Data#{updated => true},
	set_role_data(RoleId, NewData).

set_role_data(_RoleId, Data) ->
	erlang:put({?MODULE, role_data}, Data).

info(PS) ->
	case is_activity_open() of
		true ->
			role_info(PS);
		_ ->
			skip
	end.

role_info(PS) ->
	#role_state{id = RoleID, sid = RoleSid} = PS,
	RoleData = get_role_data(RoleID),
	#{posi := Posi, five_list := FiveList, barrier_list := BarrierList, evil_list := EvilList,
		pass_posi := PassPosi, step := Step, record := Record, refresh_times :=RefreshTimes} = RoleData,

	{ok, Bin} = prot_msg:encode_msg(82102, #sc_maze_five_info{posi = Posi, five_list = FiveList, barrier_list = BarrierList,
		evil_list = EvilList, pass_posi = PassPosi, step = Step, record = Record, refresh_times = RefreshTimes}),
	lib_logic_send:send_to_sid(RoleSid, Bin),
	ok.

light(PS, PosiID) ->
	case is_activity_open() of
		true ->
			case catch role_light(PS, PosiID) of
				ok -> ok;
				{ok, NewPS} -> {ok, NewPS};
				{error, Code} ->
					?ERROR_TOC(PS#role_state.id, Code)
			end;
		_ ->
			skip
	end.

role_light(PS, PosiID) ->
	#role_state{id = RoleID, sid = RoleSid} = PS,
	RoleData = get_role_data(RoleID),
	#{five_list := FiveList, barrier_list := BarrierList, evil_list := EvilList, guard := Guard,
		pass_posi := PassPosi, step := Step, record := Record, refresh_times := RefreshTimes} = RoleData,
	#{step := Step} = RoleData,
	%% 判断是否是障碍物
	case lists:member(PosiID, BarrierList) of
		true ->
			NewPassPosi = [PosiID | PassPosi],
			NewRoleData3 = RoleData#{pass_posi => NewPassPosi},
			update_role_data(RoleID, NewRoleData3),
			erlang:throw({error, ?RC_MAZE_FIVE_BARRIER});
		false -> skip
	end,
	%%  新位置是否正确
	%%  io:format("p135:[PosiID],[Posi] ~n~p~p~n",[PosiID],[Posi]),
	%%  case lists:member(PosiID, [Posi-1, Posi+1,Posi+6,Posi-6]) of
	%%    true ->
	%%       skip;
	%%    false ->
	%%      erlang:throw({error, ?RC_ACTIVITY_DICE_BOX_POS_ERROR})
	%%  end,
	#conf_sys_config{value = ConfRefreshTimes} = conf_sys_config:get(lib_maze_five_refresh_times),
	case RefreshTimes >= ConfRefreshTimes of
		true -> erlang:throw({error, ?RC_ACTIVITY_DISCOUNT_NOT_REFRESH_NUM});
		false -> skip
	end,
	NewPosi = PosiID,
	%% 判断是是否已经探索
	case lists:member(PosiID, PassPosi) of
		false ->
			%% 判断是否还有步数
			case Step > 0 of
				true ->
					NewStep = Step - 1,

					%% 判断是否是妖怪位置,和妖怪位状态
					case lists:keyfind(PosiID, 1, EvilList) of
						{ID, State} ->
							case State of
								0 ->
									NewPosi = PosiID,
									NewState = 1,
									%% NewPassPosi = [PosiID | PassPosi],
									NewEvilList = lists:keystore(ID, 1, EvilList, {ID, NewState}),
									NewRoleData = RoleData#{evil_list => NewEvilList, posi => NewPosi, pass_posi => PassPosi, step => NewStep},
									update_role_data(RoleID, NewRoleData),
									%% {ok, Bin} = prot_msg:encode_msg(82104, #sc_maze_five_light{posi_id = PosiID}),
									%% lib_logic_send:send_to_sid(RoleSid, Bin),
									{ok, Bin} = prot_msg:encode_msg(82102, #sc_maze_five_info{posi = NewPosi, five_list = FiveList, barrier_list = BarrierList,
										evil_list = NewEvilList, pass_posi = PassPosi, step = NewStep, record = Record, refresh_times = RefreshTimes}),
									lib_logic_send:send_to_sid(RoleSid, Bin),
									ok;
								1 ->
									%% 消除妖怪，发奖励。
									Index = lib_activity:get_index(?ACT_ID_MAZE_FIVE),
									%% 最小保底
									NewGuard = all_add_one_times(Guard),
									MaybeAppearIDList = maybe_appear_goods(NewGuard),
									#conf_maze_five_drop{id = ConfID, drop_id = DropID} = util:rand_by_weight([conf_maze_five:get(Index, ConfID) || ConfID <- MaybeAppearIDList], #conf_maze_five_drop.weight),
									NewNewGuard = lists:keyreplace(ConfID, 1, NewGuard, {ConfID, 0}),

									lib_drop_api:send_drop_by_id(RoleID, DropID, ?OPT_ACT_MAZE_FIVE, "maze_five"),

									NewEvilList = lists:keystore(ID, 1, EvilList, {ID, 2}),
									NewPassPosi_5 = [PosiID | PassPosi],
									NewPosi = PosiID,
									NewRoleData = RoleData#{pass_posi => NewPassPosi_5, posi => NewPosi, evil_list => NewEvilList, step => NewStep, guard => NewNewGuard},
									update_role_data(RoleID, NewRoleData),

									NewPS = lib_role_currency:add_items_notify(PS, [{?DROP_BGOLD, 50}], ?OPT_ACT_MAZE_FIVE),

									{ok, Bin} = prot_msg:encode_msg(82102, #sc_maze_five_info{posi = NewPosi, five_list = FiveList, barrier_list = BarrierList,
										evil_list = NewEvilList, pass_posi = NewPassPosi_5, step = NewStep, record = Record, refresh_times = RefreshTimes}),
									lib_logic_send:send_to_sid(RoleSid, Bin),
									{ok, NewPS}
							end;
						false ->
							NewPassPosi_6 = [PosiID | PassPosi],
							NewPosi = PosiID,

							case lists:member(PosiID, FiveList) of
								true ->
									five_reward(PS, FiveList, NewPassPosi_6);
								false -> skip
							end,

							case PosiID == 1 of
								%% 终点,发奖励
								true ->
									case RefreshTimes > 2 of
										true ->
											ok;
										false ->
											%% 发送最终大奖
											NewRefreshTimes = RefreshTimes + 1,
											Index = lib_activity:get_index(?ACT_ID_MAZE_FIVE),
											IDList = conf_maze_five_final:get_index_list(Index),
											#{drop_id := DropID} = util:rand_by_weight([conf_maze_five_final:get(Index, ID) || ID <- IDList], weight),

											lib_drop_api:send_drop_by_id(RoleID, DropID, ?OPT_ACT_MAZE_FIVE, "maze_five"),

											%% 初始化五行，妖怪，障碍的位置，经过的位置，当前的位置
											List = [ID || ID <- lists:seq(3, 28)],
											NewList = rand_N(List, 9, []),

											NewNewPassPosi = [],
											NewEvilList = [{ID, 0} || ID <- lists:sublist(NewList, 8, 2)],
											NewBarrierList = lists:sublist(NewList, 6, 2),
											NewFiveList = lists:sublist(NewList, 1, 5),
											NewRoleData = RoleData#{step => NewStep, refresh_times => NewRefreshTimes, pass_posi => NewNewPassPosi, is_get => 0,
												evil_list => NewEvilList, barrier_list => NewBarrierList, five_list => NewFiveList, posi => 30},
											update_role_data(RoleID, NewRoleData),

											{ok, Bin} = prot_msg:encode_msg(82102, #sc_maze_five_info{posi = 30, five_list = NewFiveList, barrier_list = NewBarrierList,
												evil_list = NewEvilList, pass_posi = NewNewPassPosi, step = NewStep, record = Record, refresh_times = NewRefreshTimes}),
											lib_logic_send:send_to_sid(RoleSid, Bin),
											ok
									end;
								%%普通点，发奖励
								false ->
									Index = lib_activity:get_index(?ACT_ID_MAZE_FIVE),
									%% 最小保底
									NewGuard = all_add_one_times(Guard),
									MaybeAppearIDList = maybe_appear_goods(NewGuard),
									#conf_maze_five_drop{id = ID, drop_id = DropID} = util:rand_by_weight([conf_maze_five:get(Index, ID) || ID <- MaybeAppearIDList], #conf_maze_five_drop.weight),
									%% 抽到清0
									NewNewGuard = lists:keyreplace(ID, 1, NewGuard, {ID, 0}),

									lib_drop_api:send_drop_by_id(RoleID, DropID, ?OPT_ACT_MAZE_FIVE, "maze_five"),
									{ok, Bin} = prot_msg:encode_msg(82104, #sc_maze_five_light{posi_id = PosiID}),
									lib_logic_send:send_to_sid(RoleSid, Bin),

									NewRoleData = RoleData#{posi => NewPosi, step => NewStep, pass_posi =>  NewPassPosi_6, guard => NewNewGuard},
									update_role_data(RoleID, NewRoleData),
									ok
							end
					end;

				false ->
					{error, ?RC_ACT_ANNI_NO_TIMES}
			end;
		true ->
			{ok, Bin} = prot_msg:encode_msg(82104, #sc_maze_five_light{posi_id = PosiID}),
			lib_logic_send:send_to_sid(RoleSid, Bin),
			NewRoleData2 = RoleData#{posi => PosiID},
			update_role_data(RoleID, NewRoleData2),
			ok
	end.

%% 是否发送五行的奖励n
five_reward(PS, FiveList, NewPassPosi) ->
	#role_state{id = RoleID} = PS,
	case already_five(FiveList, NewPassPosi) of
		true ->
			%% 发送五行奖励
			#conf_sys_config{value = Value} = conf_sys_config:get(lib_maze_five_reward),
			Index = lib_activity:get_index(?ACT_ID_MAZE_FIVE),
			{Index, DropID} = lists:keyfind(Index, 1, Value),
			%% lib_drop_api:send_drop_by_id(RoleID, DropID, ?OPT_ACT_MAZE_FIVE, "maze_five"),
			svr_mail:sys2p(RoleID, 92601, DropID),
			1;
		false ->
			0
	end.

%%判断是否已经五行全部走过
already_five([], _list) -> true;

already_five([ID | T], List) ->
	case lists:member(ID, List) of
		true ->
			already_five(T, List);
		_ ->
			false
	end.

get_role_data(RoleID) ->
	case erlang:get({?MODULE, role_data}) of
		undefined ->
			init_role_data(RoleID);
		Data ->
			#{guard := Guard} = Data,
			NewGuard = ?iif(Guard =:= [], make_guard(), Guard),
			RoleData = Data#{guard => NewGuard},
			RoleData
	end.

save(PS, _) ->
	case is_activity_open() of
		true ->
			save__(PS);
		_ ->
			skip
	end.

save__(PS) ->
	#role_state{id = RoleID} = PS,
	RoleData = get_role_data(RoleID),
	#{updated := Updated} = RoleData,
	?iif(Updated, save_data(RoleID, RoleData), skip).

save_data(RoleID, RoleData) ->
	#{posi := Posi, evil_list := EvilList, five_list := FiveList, barrier_list := BarrierList, pass_posi := PassPosi, step := Step, record := Record, last_check := LastCheck, refresh_times := RefreshTimes, guard :=  Guard, is_get := IsGet} = RoleData,
	save_role_data(RoleID, Posi, FiveList, BarrierList, EvilList, PassPosi, Step, Record, LastCheck, RefreshTimes, Guard, IsGet).

is_activity_open() ->
	lib_activity:is_activity_ongoing(?ACT_ID_MAZE_FIVE).

cross_day_reset(PS) ->
	case is_activity_open() of
		true ->
			do_cross_day_reset(PS);
		_ ->
			skip
	end.

do_cross_day_reset(PS) ->
	#role_state{id = RoleID, sid = RoleSid} = PS,
	RoleData = get_role_data(RoleID),
	#{posi := Posi, five_list := FiveList, barrier_list := BarrierList, evil_list := EvilList,
		pass_posi := PassPosi, step := Step} = RoleData,
	%% 剩余步数加1
	NewStep = Step + 1,
	NewRefreshTimes = 0,
	%% 每日消费清空
	NewRecord = 0,
	{ok, Bin} = prot_msg:encode_msg(82102, #sc_maze_five_info{posi = Posi, five_list = FiveList, barrier_list = BarrierList,
		evil_list = EvilList, pass_posi = PassPosi, step = NewStep, record = NewRecord, refresh_times = NewRefreshTimes}),
	lib_logic_send:send_to_sid(RoleSid, Bin),
	NewRoleData = RoleData#{last_check => time:unixtime(), step => NewStep, record => NewRecord, refresh_times => NewRefreshTimes},
	update_role_data(RoleID, NewRoleData),
	ok.

%% 消费元宝增加步数
handle_info({consume, CostGold}, PS) ->
	consume(PS, CostGold);
handle_info(_Info, _PS) ->
	ok.
consume(PS, _Gold) ->
	case is_activity_open() of
		true ->
			#role_state{id = RoleID, sid = RoleSid} = PS,
			RoleData = get_role_data(RoleID),
			#{posi := Posi, five_list := FiveList, barrier_list := BarrierList, evil_list := EvilList,
				pass_posi := PassPosi, step := Step, record := Record, refresh_times :=RefreshTimes} = RoleData,
			NewRecord = Record + _Gold,
			#conf_sys_config{value = NeedCost} = conf_sys_config:get(lib_maze_five_cost),
			Mod = Record rem NeedCost,
			ModCostGold = Mod + _Gold,
			AddStep = ModCostGold div NeedCost,
			NewStep = Step + AddStep,
			{ok, Bin} = prot_msg:encode_msg(82102, #sc_maze_five_info{posi = Posi, five_list = FiveList, barrier_list = BarrierList,
				evil_list = EvilList, pass_posi = PassPosi, step = NewStep, record = NewRecord, refresh_times = RefreshTimes}),
			lib_logic_send:send_to_sid(RoleSid, Bin),
			%% 更新
			NewRoleData = RoleData#{step => NewStep, record => NewRecord},
			update_role_data(RoleID, NewRoleData);
		_ ->
			skip
	end.


-define(SQL_GET_ROLE_DATA, <<"select posi, five_list, barrier_list, evil_list, pass_posi, step, record, last_check, refresh_times, guard,is_get from role_maze_five where role_id = ~w">>).
get_role_db_data(RoleID) ->
	SQL = io_lib:format(?SQL_GET_ROLE_DATA, [RoleID]),
	case ?DB:get_row(SQL) of
		[Posi, FiveList, BarrierList, EvilList, PassPosi, Step, Record, LastCheck, RefreshTimes, Guard, IsGet] ->
			{
				Posi,
				type:bitstring_to_term(FiveList),
				type:bitstring_to_term(BarrierList),
				type:bitstring_to_term(EvilList),
				type:bitstring_to_term(PassPosi),
				Step,
				Record,
				LastCheck,
				RefreshTimes,
				type:bitstring_to_term(Guard),
				IsGet
			};
		%% 初始化
		[] ->
			List = [ID || ID <- lists:seq(3, 28)],
			NewList = rand_N(List, 9, []),
			{
				30,
				lists:sublist(NewList, 1, 5),  %% F
				lists:sublist(NewList, 6, 2),  %% B
				[{ID, 0} || ID <- lists:sublist(NewList, 8, 2)],  % E
				[],
				1,
				0,
				0,
				0,
				make_guard(),
				0
			}
	end.

-define(SQL_SET_ROLE_DATA, <<"replace into role_maze_five(role_id, posi, five_list, barrier_list, evil_list, pass_posi, step, record, last_check, refresh_times, guard, is_get)values(~w, ~w, '~s', '~s', '~s', '~s',~w, ~w, ~w, ~w, '~s', ~w) ">>).
save_role_data(RoleID, Posi, FiveList, BarrierList, EvilList, PassPosi, Step, Record, LastCheck, RefreshTimes, Guard, IsGet) ->
	SQL = io_lib:format(?SQL_SET_ROLE_DATA,
		[
			RoleID,
			Posi,
			type:term_to_bitstring(FiveList),
			type:term_to_bitstring(BarrierList),
			type:term_to_bitstring(EvilList),
			type:term_to_bitstring(PassPosi),
			Step,
			Record,
			LastCheck,
			RefreshTimes,
			type:term_to_bitstring(Guard),
			IsGet
		]),
	?DB:execute(SQL).

%%从列表中随机抽出个数为PickNum的列表
rand_N(_Tuples, 0, Ret) ->
	Ret;
rand_N([], _PickNum, Ret) ->
	Ret;

rand_N(Tuples, PickNum, Ret) ->
	PickOne = random:uniform(26) + 2,
	case lists:member(PickOne, Tuples) of
		true ->
			LeftTuples = lists:delete(PickOne, Tuples),
			rand_N(LeftTuples, PickNum - 1, [PickOne | Ret]);

		false ->
			LeftTuples = lists:delete(PickOne, Tuples),
			rand_N(LeftTuples, PickNum, Ret)
	end.


%% 最小保底
%% 生成概率保护
make_guard() ->
	ConfIndex = lib_activity:get_index(?ACT_ID_MAZE_FIVE),
	IDList = conf_maze_five:get_index_list(ConfIndex),
	lists:map(fun(ID) -> {ID, 0} end, IDList).

%% 所有的物品刷新次数+1
all_add_one_times(Guard) ->
	ConfIndex = lib_activity:get_index(?ACT_ID_MAZE_FIVE),
	IDList = conf_maze_five:get_index_list(ConfIndex),
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
	ConfIndex = lib_activity:get_index(?ACT_ID_MAZE_FIVE),
	lists:foldl(
		fun({ID, RefreshTimes}, List) ->
			#conf_maze_five_drop{min_times = MinTimes} = conf_maze_five:get(ConfIndex, ID),
			if
				RefreshTimes >= MinTimes ->
					[ID | List];
				true ->
					List
			end
		end, [], Guard).
