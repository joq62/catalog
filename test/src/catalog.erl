%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%% 
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(catalog). 

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").

-include("catalog.hrl").



%% API

-export([
	
	]).

-export([

	]).

%% admin




-export([
	 start/0,
	 ping/0,
	 stop/0
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
		     
-record(state, {
		
	        
	       }).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Return all repos   
%% 
%% @end
%%--------------------------------------------------------------------
-spec repos() -> 
	  {ok,ListOfRepos :: term()} | {error, Reason :: term()}.
repos() ->
    gen_server:call(?SERVER,{repos},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Returns the path to "ebin" dir and if needed "priv" dir 
%% related to Application Id  
%% 
%% @end
%%--------------------------------------------------------------------
-spec paths(ApplicationId :: string()) -> 
	  {ok,ListOfPaths :: term()} | {error, Reason :: term()}.
paths(ApplicationId) ->
    gen_server:call(?SERVER,{paths,ApplicationId},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Resturn the app for application ApplicationId  
%% 
%% @end
%%--------------------------------------------------------------------
-spec app(ApplicationId::string()) -> 
	  {ok,App} | {error, Reason :: term()}.
app(ApplicationId) ->
    gen_server:call(?SERVER,{app,ApplicationId},infinity).
%%--------------------------------------------------------------------
%% @doc
%% git merge for the repo LocalRepo 
%% 
%% @end
%%--------------------------------------------------------------------
-spec merge(LocalRepo::string()) -> ok | 
	  {error, Reason :: term()}.
merge(LocalRepo) ->
    gen_server:call(?SERVER,{merge,LocalRepo},infinity).

%%--------------------------------------------------------------------
%% @doc
%% stops all kubeletes and deletes their dirs 
%% 
%% @end
%%--------------------------------------------------------------------
-spec delete_cluster(ClusterId::string()) -> ok | 
	  {error, Error :: term()}.
delete_cluster(ClusterId) ->
    gen_server:call(?SERVER,{delete_cluster,ClusterId},infinity).

%%--------------------------------------------------------------------
%% @doc
%%  
%% 
%% @end
%%--------------------------------------------------------------------
start()->
    application:start(?MODULE).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
kill()->
    gen_server:call(?SERVER, {kill},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec ping() -> pong | Error::term().
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%stop()-> gen_server:cast(?SERVER, {stop}).
stop()-> gen_server:stop(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.

init([]) ->
    io:format("Environment ~p~n",[?ENVIRONMENT]),

    
%    ?LOG_NOTICE("Server started ",[?MODULE]),
    {ok, #state{
	    mode=?ENVIRONMENT
	    
	   },0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.

    
handle_call({new_cluster,ClusterId,HostNameNumWorkers}, _From, State) 
  when State#state.mode==test ->
  
    Result=try lib_control:new_cluster(HostNameNumWorkers) of
	       {ok,CreateResult}->
		   {ok,CreateResult};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       error:Reason:Stacktrace->
		   {error,Reason,Stacktrace,?MODULE,?LINE};
	       throw:Reason:Stacktrace->
		   {throw,Reason,Stacktrace,?MODULE,?LINE};
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,KubeletList}->
		  io:format("CreateResult ~p~n",[{KubeletList,?MODULE,?LINE}]),
		  NewState=State,
		  {ok,KubeletList};
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  {error,ErrorEvent}
	  end,
    {reply, Reply, NewState};
 

%%--------------------------------------------------------------------



handle_call({ping}, _From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(UnMatchedSignal, From, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal, From,?MODULE,?LINE}]),
    Reply = {error,[unmatched_signal,UnMatchedSignal, From]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({stop}, State) ->
    
    {stop,normal,ok,State};

handle_cast(UnMatchedSignal, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.

handle_info(timeout, State) 
  when State#state.mode==production ->
    io:format("timeout ~p~n",[{State#state.mode,?MODULE,?LINE}]),
   
    
    {noreply, State};


handle_info(Info, State) ->
    io:format("unmatched_signal ~p~n",[{Info,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
