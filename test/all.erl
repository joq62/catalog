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
-module(all).      
 
-export([start/0]).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok=setup(),
    ok=git_status(),

    io:format("Test OK !!! ~p~n",[?MODULE]),
%    timer:sleep(1000),
%    init:stop(),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
git_status()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    LocalRepo="/home/joq62/erlang/function_test/catalog/adder",
 %   LocalRepo="/home/joq62/erlang/dev/adder",
    R=lib_catalog:is_up_to_date(LocalRepo),
    io:format("Repo status ~p~n",[{R,?MODULE,?FUNCTION_NAME}]),
    R1=lib_catalog:merge(LocalRepo),
    io:format("Repo merge and status ~p~n",[{R1,?MODULE,?FUNCTION_NAME}]),
    R2=lib_catalog:get_tags(LocalRepo),
    io:format("Repo tagss ~p~n",[{R2,?MODULE,?FUNCTION_NAME}]),
    

    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    ok=application:start(catalog),
       
    pong=catalog:ping(),
    ok.
