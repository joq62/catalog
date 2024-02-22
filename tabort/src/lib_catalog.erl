%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_catalog).
  

-define(UpToDate,"Up to date").
-define(NotUpToDate,"Not up to date").
 
%% API
-export([
	 is_up_to_date/1,
	 get_tags/1,
	 merge/1
	 
	]).

-export([

	]).

%%%===================================================================
%%% API
%%%===================================================================


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Application  part Start
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

get_tags(LocalRepo)->
    TagString=os:cmd("git -C "++LocalRepo++" "++"tag"),
    T1=[S||S<-string:split(TagString, "\n", all),
	     []=/=S],
    Tags=lists:reverse(lists:sort(T1)),
    Tags.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

merge(LocalRepo)->
    Result=case is_up_to_date(LocalRepo) of
	       false->
		   os:cmd("git -C "++LocalRepo++" "++"merge  "),
		   ok;
	       true->
		   {error,["Already updated ",LocalRepo]}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_up_to_date(LocalRepo)->

    Fetch=os:cmd("git -C "++LocalRepo++" "++"fetch origin "),
    Status=os:cmd("git -C "++LocalRepo++" status -uno | grep -q 'Your branch is up to date'  && echo Up to date || echo Not up to date"),
    [FilteredGitStatus]=[S||S<-string:split(Status, "\n", all),
			  []=/=S],
    Result=case FilteredGitStatus of
	       ?UpToDate->
		   true;
	       ?NotUpToDate->
		   false
	   end,
    Result.



%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
