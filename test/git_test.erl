%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(git_test).      
 
-export([start/0]).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("catalog.hrl").

-define(TestApplicationId,"log").
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok=setup(),
    ok=git_repo(),
 
    io:format("Test OK !!! ~p~n",[?MODULE]),
    timer:sleep(1000),
    init:stop(),
    ok.


    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
git_repo()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {error,{error,["RepoDir doesnt exists, need to clone","catalog/catalog_specs"]}}=catalog:is_repo_updated(),
    ok=catalog:clone_repo(),
    true=catalog:is_repo_updated(),
    {error,{error,["Already updated ","catalog/catalog_specs"]}}=catalog:update_repo(),
    
   [
    "adder","controller","deploy","divi","host",
    "log","resource_discovery","worker"
   ]=lists:sort(catalog:get_all_ids()),
   
    {ok,Map}=catalog:get_map(?TestApplicationId),
   
    [
     {app,log},{application_name,"log"},{erl_args," "},
     {git,"https://github.com/joq62/log.git"},
     {id,"log"},{vsn,"0.1.0"}
    ]=lists:sort(maps:to_list(Map)),

    {ok,?TestApplicationId}=catalog:get_info(id,?TestApplicationId),
    {ok,"log"}=catalog:get_info(application_name,?TestApplicationId),
    {ok,"0.1.0"}=catalog:get_info(vsn,?TestApplicationId),
    {ok,log}=catalog:get_info(app,?TestApplicationId),
    {ok," "}=catalog:get_info(erl_args,?TestApplicationId),
    {ok,"https://github.com/joq62/log.git"}=catalog:get_info(git,?TestApplicationId),

    {error,{badkey,glurk},_,_,_}=catalog:get_info(glurk,?TestApplicationId),
   {error,["ApplicationId doens't exists",glurk]}=catalog:get_info(git,glurk),
    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    file:del_dir_r(?MainDir),
    file:make_dir(?MainDir),
    ok.
