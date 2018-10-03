%%%-------------------------------------------------------------------
%%% @author heyisheng
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%   登录签到
%%% @end
%%% Created : 02. 八月 2018 17:42
%%%-------------------------------------------------------------------
-module(lib_anni_login).
-author("yisheng").

-include("common.hrl").
-include("role.hrl").
-include("drop.hrl").
-include("ret_num.hrl").
-include("activity.hrl").
-include("reward.hrl").
-include("op_type.hrl").
-include("prot_resolve_record/prot_819.hrl").

%% API
-export([
	init/1,
	info/1,
	cross_day_reset/1,
	save/2,
	erase_data/1,
	final/1,
	reward/2,
	start_activity/1,
	stop_activity/2,
	stop_god_login_activity/2
]).

start_activity(_Activity) ->
	ok.

stop_activity(_Activity, _) ->
%%  svr_chat:info_online({'set_data', [{stop_anni_login_activity, Activity}]}),
	erlang:spawn(
		fun() ->
			timer:sleep(10000),
			?DB:execute("DELETE FROM role_anni_login")
		end),
	ok.

stop_god_login_activity(PS, _Activity) ->
	erase_data(PS#role_state.id),
	ok.

init(PS) ->
	case is_activity_open() of
		true ->
			init_role_data(PS#role_state.id);
		_ ->
			skip
	end.

init_role_data(RoleID) ->
	{RewardList, LastCheck, IsGet, Day, Flag} = get_role_db_data(RoleID),
	{NewDay, NewLastCheck, NewIsGet, Updated} = get_new_data(Day, LastCheck, IsGet),
	RoleData = #{reward_list => RewardList, last_check => NewLastCheck, is_get => NewIsGet, day => NewDay, flag =>Flag, updated => Updated},
	?iif(Updated, update_role_data(RoleID, RoleData), set_role_data(RoleID, RoleData)),
	RoleData.

get_new_data(Day, LastCheck, IsGet) ->
	case time:is_today(LastCheck) of
		false ->
			NewIsGet = 0,
			%%判断时候连续登录
			case time:is_yesterday(LastCheck) of
				false ->
					%%连续登录的天数为1
					NewDay = 1;
				_ ->
					%%连续登录天数加一
					NewDay = Day + 1
			end,
			{NewDay, time:unixtime(), NewIsGet, true};
		_ ->
			{Day, LastCheck, IsGet, false}
	end.

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
	#{reward_list := RewardList, is_get := IsGet, day := Day, flag := Flag} = RoleData,
	{ok, Bin} = prot_msg:encode_msg(81926, #sc_anni_login_info{reward_list = RewardList, flag = Flag, is_get = IsGet, day = Day}),
	lib_logic_send:send_to_sid(RoleSid, Bin),
	ok.

%% 领取每日蛋糕
reward(PS, Id) ->
	case is_activity_open() of
		true ->
			case catch role_reward(PS, Id) of
				ok -> ok;
				{error, Code} ->
					?ERROR_TOC(PS#role_state.id, Code)
			end;
		_ ->
			skip
	end.

role_reward(PS, Id) ->
	#role_state{id = RoleID, sid = RoleSid} = PS,
	RoleData = get_role_data(RoleID),
	#{reward_list := RewardList, is_get := IsGet} = RoleData,
	%%判断今天是否领取
	case IsGet of
		0 ->
			case lists:keyfind(Id, 1, RewardList) of
				{Id, State} ->
					case State of
						0 ->
							skip;
						_ ->
							erlang:throw({error, ?RC_ACT_ANNI_ALREADY_ACCEPT})
					end;
				_ ->
					erlang:throw({error, ?RC_ACT_ANNI_CANT_ACCEPT})
			end;
		_ ->
			erlang:throw({error, ?RC_ACT_ANNI_ALREADY_ACCEPT})
	end,
	%% 获取活动的Index
	Index = lib_activity:get_index(?ACT_ID_ANNI_LOGIN),
	Conf = conf_anni_login:get(Index),
	Conf =/= undefined orelse erlang:throw({error, ?RC_GUESS_FINGER_ID_ERROR}),
	#{login_reward := LoginReward} = Conf,
	%% 获取DropID
	{Id, DropID} = lists:keyfind(Id, 1, LoginReward),
	NewRewardList = lists:keystore(Id, 1, RewardList, {Id, 1}),
	NewIsGet = 1,
	NewRoleData = RoleData#{reward_list => NewRewardList, is_get => NewIsGet},
	set_role_data(RoleID, NewRoleData),
	save_data(RoleID, NewRoleData),
	lib_drop_api:send_drop_by_id(RoleID, DropID, ?OPT_ACT_ANNI_LOGIN, "anni_login"),
	{ok, Bin} = prot_msg:encode_msg(81928, #sc_anni_login_reward{id = Id}),
	lib_logic_send:send_to_sid(RoleSid, Bin),
	ok.

%% 终极大奖
final(PS) ->
	case is_activity_open() of
		true ->
			case catch role_final(PS) of
				ok -> ok;
				{error, Code} ->
					?ERROR_TOC(PS#role_state.id, Code)
			end;
		_ ->
			skip
	end.

role_final(PS) ->
	#role_state{id = RoleID, sid = RoleSid} = PS,
	RoleData = get_role_data(RoleID),
	#{flag := Flag, day := Day} = RoleData,
	Flag =:= 0 orelse erlang:throw({error, ?RC_ACT_ANNI_ALREADY_ACCEPT}),
	%% 获取活动的Index
	Index = lib_activity:get_index(?ACT_ID_ANNI_LOGIN),
	Conf = conf_anni_login:get(Index),
	Conf =/= undefined orelse erlang:throw({error, ?RC_GUESS_FINGER_ID_ERROR}),
	#{final_reward := DropID, need_day := NeedDay} = Conf,
	Day >= NeedDay orelse erlang:throw({error, ?RC_ACT_ANNI_CANT_ACCEPT}),
	NewFlag = 1,
	NewRoleData = RoleData#{flag => NewFlag},
	set_role_data(RoleID, NewRoleData),
	save_data(RoleID, NewRoleData),
	lib_drop_api:send_drop_by_id(RoleID, DropID, ?OPT_ACT_ANNI_LOGIN, "anni_login"),
	{ok, Bin} = prot_msg:encode_msg(81930, #sc_anni_login_final{flag = NewFlag}),
	lib_logic_send:send_to_sid(RoleSid, Bin),
	ok.

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
	#{reward_list := RewardList, day := Day, flag := Flag} = RoleData,
	NewDay = Day + 1,
	NewIsGet = 0,
	{ok, Bin} = prot_msg:encode_msg(81926, #sc_anni_login_info{reward_list = RewardList, flag = Flag, is_get = NewIsGet, day = NewDay}),
	lib_logic_send:send_to_sid(RoleSid, Bin),
	NewRoleData = RoleData#{last_check => time:unixtime(), day => NewDay, is_get =>NewIsGet},
	update_role_data(RoleID, NewRoleData),
	ok.

update_role_data(RoleId, Data) ->
	NewData = Data#{updated => true},
	set_role_data(RoleId, NewData).

set_role_data(_RoleId, Data) ->
	erlang:put({?MODULE, role_data}, Data).

get_role_data(RoleID) ->
	case erlang:get({?MODULE, role_data}) of
		undefined ->
			init_role_data(RoleID);
		Data ->
			Data
	end.

erase_data(_RoleId) ->
	erase({?MODULE, role_data}).

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
	#{reward_list := RewardList, last_check := LastCheck, is_get := IsGet, day := Day, flag := Flag} = RoleData,
	save_role_data(RoleID, RewardList, LastCheck, IsGet, Day, Flag).

is_activity_open() ->
	lib_activity:is_activity_ongoing(?ACT_ID_ANNI_LOGIN).

-define(SQL_GET_ROLE_DATA, <<"select reward_list, last_check, is_get, day, flag from role_anni_login where role_id =~w">>).
get_role_db_data(RoleID) ->
	SQL = io_lib:format(?SQL_GET_ROLE_DATA, [RoleID]),
	case ?DB:get_row(SQL) of
		[RewardList, LastCheck, IsGet, Day, Flag] ->
			{type:bitstring_to_term(RewardList), LastCheck, IsGet, Day, Flag};
		[] ->
			#{login_reward := Reward} = conf_anni_login:get(1),
			Length = length(Reward),
			RewardList = [{ID, 0} || ID <- lists:seq(1, Length)],
			{RewardList, 0, 0, 0, 0}
	end.

-define(SQL_SET_ROLE_DATA, <<"replace into role_anni_login(role_id, reward_list, last_check, is_get, day, flag) values(~w, '~s' ,~w, ~w, ~w, ~w)">>).
save_role_data(RoleID, RewardList, LastCheck, IsGet, Day, Flag) ->
	SQL = io_lib:format(?SQL_SET_ROLE_DATA, [RoleID, type:term_to_bitstring(RewardList), LastCheck, IsGet, Day, Flag]),
	?DB:execute(SQL).

