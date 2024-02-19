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

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok=setup(),
    ok=git_normal(),

    io:format("Test OK !!! ~p~n",[?MODULE]),
%    timer:sleep(1000),
%    init:stop(),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
git_normal()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {error,{error,["Inventory doesnt exists, need to clone"]}}=catalog:is_inventory_updated(),
    ok=catalog:clone_inventory(),
    true=catalog:is_inventory_updated(),

    {ok,InventoryList}=catalog:get_inventory(),
     [
      "adder","divi","etcd","log","resource_discovery"
     ]=lists:sort(InventoryList),

    {error,{error,["Already updated ","inventory"]}}=catalog:update_inventory(),

    
 

%   {error,Reason}=catalog:clone_inventory(),

    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    file:del_dir_r(?InventoryDir),
    
    ok.
