%%%-------------------------------------------------------------------
%%% @author Данил
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Май 2016 17:40
%%%-------------------------------------------------------------------
-module(filesystem).
-author("Данил").

%% API
-export([startFS/0,
  recurserM/1]).

recurserM(Dir)->
  {ok, Filenames}=file:list_dir(Dir),
  processOneM(Dir,Filenames).


processOneM(Parent, [First|Next])->
  FullName=Parent++"/"++First,
  IsDir=filelib:is_dir(FullName),
  if
    IsDir==true -> recurserM(FullName);
    IsDir==false -> io:format(FullName++"~n"),
      Master = whereis(superfilesystem),  %%%superfilesystem - имя процесса aggregationProcess
      Master!{FullName,erlang:spawn(fun() ->processBody(FullName) end)}
  end,
  processOneM(Parent,Next);
processOneM(_Parent,[])->ok.

processBody(FullName) ->
  receive
    {delete,PidWorkerOnMainActor} ->
      Rez =  file:delete(FullName),
      if
        Rez == ok ->  PidWorkerOnMainActor ! {deleted,node(),FullName}, exit(self(),normal);
        true -> PidWorkerOnMainActor ! {error}
      end;
    {rename, NewName, PidWorkerOnMainActor} ->
      Rez = file:rename(FullName,NewName),
      if
        Rez == ok ->  PidWorkerOnMainActor ! {renamed,node(),FullName,NewName},  processBody(NewName);
        true -> PidWorkerOnMainActor ! {error}
      end;
    {copy, NewPlace, PidWorkerOnMainActor} ->
      {Rez,_} = file:copy(FullName,NewPlace),
      if
        Rez == ok  ->
          PidWorkerOnMainActor ! {copied,node(),FullName,NewPlace,erlang:spawn(fun() ->processBody(NewPlace) end)};
        true -> PidWorkerOnMainActor ! {error}
      end


  end,
  processBody(FullName).

aggregationProcess(MapAllFiles)->
  receive
    {Key,Value} ->
      AllFiles = maps:put(Key,Value,MapAllFiles),
      aggregationProcess(AllFiles);
    ok->
      ControlPr = spawn(fun() -> controlProcess(MapAllFiles) end),
      register(getFS,ControlPr),
      exit(self(),normal)
  end,
  aggregationProcess(MapAllFiles).

controlProcess(MapAllFiles)->
  receive
    {getFS,PidSender} ->
      io:format("FS connected ~n"),
      PidSender ! {node(),MapAllFiles},
      exit(self(),normal)
  end,
  controlProcess(MapAllFiles).

startFS()->
  AgrProcPid = spawn(fun()-> aggregationProcess(maps:new()) end),
  register(superfilesystem,AgrProcPid),
  AgrProcPid ! recurserM(".").
