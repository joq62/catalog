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
-include("catalog.resource_discovery").



%% API

-export([
	 is_repo_updated/0,
	 update_repo/0,
	 clone_repo/0
	]).

-export([
	 get_application_paths/1,
	 get_application_app/1,
	 is_application_repo_updated/1,
	 update_application_repo/1,
	 clone_application_repo/1
	]).
-export([
	 get_info/2,
	 get_all_ids/0,
	 get_maps/0,
	 get_map/1
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
		
		main_dir,
	        repo_dir,
		application_dir,
		spec_maps,
	        repo_git
	        
	       }).


%%%===================================================================
%%% API
%%%===================================================================

%%********************* Appl *****************************************
%%--------------------------------------------------------------------
%% @doc
%% get the full path to ebin and if presence the priv dirs to application
%% ApplId  
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_application_app(ApplicationId :: string()) -> 
	  {ok,App :: atom()} | {error, Reason :: term()}.
get_application_app(ApplicationId) ->
    gen_server:call(?SERVER,{get_application_app,ApplicationId},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get the full path to ebin and if presence the priv dirs to application
%% ApplId  
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_application_paths(ApplicationId :: string()) -> 
	  {ok,ListOfPAths:: term()} | {error, Reason :: term()}.
get_application_paths(ApplicationId) ->
    gen_server:call(?SERVER,{get_application_paths,ApplicationId},infinity).

%%--------------------------------------------------------------------
%% @doc
%%  
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_info(Key:: atom(),ApplicationId :: string()) -> 
	  {ok,Value:: term()} | {error, Reason :: term()}.
get_info(Key,ApplicationId) ->
    gen_server:call(?SERVER,{get_info,Key,ApplicationId},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get the full path to ebin and if presence the priv dirs to application
%% ApplId  
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_all_ids() -> 
	  {ok,ApplicationIdList :: term()} | {error, Reason :: term()}.
get_all_ids() ->
    gen_server:call(?SERVER,{get_all_ids},infinity).
%%--------------------------------------------------------------------
%% @doc
%% get the full path to ebin and if presence the priv dirs to application
%% ApplId  
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_maps() -> 
	  {ok,ApplicationMapsList :: term()} | {error, Reason :: term()}.
get_maps() ->
    gen_server:call(?SERVER,{get_maps},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get the full path to ebin and if presence the priv dirs to application
%% ApplId  
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_map(ApplicationId :: string()) -> 
	  {ok,ApplicationMap :: map()} | {error, Reason :: term()}.
get_map(ApplicationId) ->
    gen_server:call(?SERVER,{get_map,ApplicationId},infinity).


%%********************* Repo ************************************


%	 is_application_repo_updated/1,
%	 update_application_repo/1,
% clone_application_repo/1


%%--------------------------------------------------------------------
%% @doc
%%    
%% 
%% @end
%%--------------------------------------------------------------------
-spec is_application_repo_updated(ApplicationId :: string()) -> 
	  true | false | {error,Reason :: term()}.

is_application_repo_updated(ApplicationId) ->
    gen_server:call(?SERVER,{is_application_repo_updated,ApplicationId},infinity).

%%--------------------------------------------------------------------
%% @doc
%% repo   
%% 
%% @end
%%--------------------------------------------------------------------
-spec update_application_repo(ApplicationId :: string()) -> 
	  ok | {error, Reason :: term()}.
update_application_repo(ApplicationId) ->
    gen_server:call(?SERVER,{update_application_repo,ApplicationId},infinity).

%%--------------------------------------------------------------------
%% @doc
%%    
%% 
%% @end
%%--------------------------------------------------------------------
-spec clone_application_repo(ApplicationId :: string()) -> 
	  ok | {error, Reason :: term()}.
clone_application_repo(ApplicationId) ->
    gen_server:call(?SERVER,{clone_application_repo,ApplicationId},infinity).

%%--------------------------------------------------------------------
%% @doc
%%    
%% 
%% @end
%%--------------------------------------------------------------------
-spec is_repo_updated() -> 
	  true | false | {error,Reason :: term()}.

% {error,["Inventory doesnt exists, need to clone"]} .
is_repo_updated() ->
    gen_server:call(?SERVER,{is_repo_updated},infinity).

%%--------------------------------------------------------------------
%% @doc
%% repo   
%% 
%% @end
%%--------------------------------------------------------------------
-spec update_repo() -> 
	  ok | {error, Reason :: term()}.
update_repo() ->
    gen_server:call(?SERVER,{update_repo},infinity).

%%--------------------------------------------------------------------
%% @doc
%%    
%% 
%% @end
%%--------------------------------------------------------------------
-spec clone_repo() -> 
	  ok | {error, Reason :: term()}.
clone_repo() ->
    gen_server:call(?SERVER,{clone_repo},infinity).

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
    
    
%    ?LOG_NOTICE("Server started ",[?MODULE]),
    {ok, #state{
	    main_dir=?MainDir,
	    repo_dir=?RepoDir,
	    application_dir=?ApplicationDir,
	    spec_maps=[],
	    repo_git=?RepoGit
	  
	    
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

%%********************* Appl *****************************************

handle_call({get_application_paths,ApplicationId}, _From, State) ->
    ApplicationDir=State#state.application_dir,
    SpecMaps=State#state.spec_maps,
    Result=try lib_catalog:get_application_paths(ApplicationDir,ApplicationId,SpecMaps) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,Paths}->
		  {ok,Paths};
	      ErrorEvent->
		  ErrorEvent
	  end,
    {reply, Reply, State};

handle_call({get_application_app,ApplicationId}, _From, State) ->
    ApplicationDir=State#state.application_dir,
    SpecMaps=State#state.spec_maps,
    Result=try lib_catalog:get_application_app(ApplicationDir,ApplicationId,SpecMaps) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,App}->
		  {ok,App};
	      ErrorEvent->
		  ErrorEvent
	  end,
    {reply, Reply, State};

handle_call({get_maps}, _From, State) ->
    Reply=State#state.spec_maps,
    {reply, Reply, State};

handle_call({get_all_ids}, _From, State) ->
    Reply= [maps:get(id,Map)||Map<-State#state.spec_maps],
    {reply, Reply, State};

handle_call({get_map,ApplicationId}, _From, State) ->
    SpecMaps=State#state.spec_maps,
    Result=try lib_catalog:get_map(ApplicationId,SpecMaps) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,Map}->
		  {ok,Map};
	      ErrorEvent->
		  ErrorEvent
	  end,
    {reply, Reply, State};

handle_call({get_info,Key,ApplicationId}, _From, State) ->
    SpecMaps=State#state.spec_maps,
    Result=try lib_catalog:get_info(Key,ApplicationId,SpecMaps) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,Value}->
		  {ok,Value};
	      ErrorEvent->
		  ErrorEvent
	  end,
    {reply, Reply, State};

%%********************* Repo ************************************

handle_call({is_application_repo_updated,ApplicationId}, _From, State) ->
    ApplicationDir=State#state.application_dir,
    SpecMaps=State#state.spec_maps,
    Result=try lib_catalog:is_application_repo_updated(ApplicationDir,ApplicationId,SpecMaps) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,IsUpdated}->
		  %io:format("IsUpdated ~p~n",[{IsUpdated,?MODULE,?LINE}]),
		  NewState=State,
		  IsUpdated;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  ErrorEvent
	  end,
    {reply, Reply, NewState};

handle_call({update_application_repo,ApplicationId}, _From, State) ->
    ApplicationDir=State#state.application_dir,
    SpecMaps=State#state.spec_maps,
    Result=try lib_catalog:update_application_repo(ApplicationDir,ApplicationId,SpecMaps) of
	       ok->
		   ok;
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok->
		  %io:format("UpdateResult ~p~n",[{UpdateResult,?MODULE,?LINE}]),
		  NewState=State,
		  ok;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  ErrorEvent
	  end,
    {reply, Reply, NewState};

handle_call({clone_application_repo,ApplicationId}, _From, State) ->
    ApplicationDir=State#state.application_dir,
    SpecMaps=State#state.spec_maps,
    Result=try lib_catalog:clone_application_repo(ApplicationDir,ApplicationId,SpecMaps) of
	       ok->
		   ok;
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok->
		%  io:format("CloneResult ~p~n",[{ok,?MODULE,?LINE}]),
		  NewState=State,
		  ok;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		 ErrorEvent
	  end,
    {reply, Reply, NewState};

handle_call({is_repo_updated}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try lib_catalog:is_repo_updated(RepoDir) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,IsUpdated}->
		  %io:format("IsUpdated ~p~n",[{IsUpdated,?MODULE,?LINE}]),
		  NewState=State,
		  IsUpdated;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  ErrorEvent
	  end,
    {reply, Reply, NewState};

handle_call({update_repo}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try lib_catalog:update_repo(RepoDir) of
	       ok->
		   ok;
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok->
		  %io:format("UpdateResult ~p~n",[{UpdateResult,?MODULE,?LINE}]),
		  NewState=State,
		  ok;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  ErrorEvent
	  end,
    {reply, Reply, NewState};

handle_call({clone_repo}, _From, State) ->
    RepoDir=State#state.repo_dir,
    RepoGit=State#state.repo_git,
    Result=try lib_catalog:clone_repo(RepoDir,RepoGit) of
	       ok->
		   ok;
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok->
		%  io:format("CloneResult ~p~n",[{ok,?MODULE,?LINE}]),
		  NewState=State,
		  ok;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  ErrorEvent
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

handle_info(timeout, State) ->
%    io:format("timeout ~p~n",[{?MODULE,?LINE}]),
    RepoDir=State#state.repo_dir,
    RepoGit=State#state.repo_git,
    ApplicationDir=State#state.application_dir,
    Result=try lib_catalog:check_update_repo_return_maps(RepoDir,RepoGit,ApplicationDir) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    NewState=case Result of
		 {ok,SpecMaps}->
		  %   io:format("SpecMaps ~p~n",[{SpecMaps,?MODULE,?LINE}]),
		     State#state{spec_maps=SpecMaps};
		 ErrorEvent->
		     io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		     State
	     end,
    
    initial_trade_resources(),

    {noreply, NewState};


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
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
initial_trade_resources()->
    [rd:add_local_resource(ResourceType,Resource)||{ResourceType,Resource}<-?LocalResourceTuples],
    [rd:add_target_resource_type(TargetType)||TargetType<-?TargetTypes],
    rd:trade_resources(),
    timer:sleep(3000),
    ok.
