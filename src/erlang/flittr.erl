-module(flittr).

-export([get_photos/2,search_photos/4,query/1]).

get_photos(UserId, ApiKey)->
  Url=lists:flatten(["http://api.flickr.com/services/rest/?method=flickr.people.getPublicPhotos&api_key=", ApiKey, "&user_id=", UserId, "&format=rest"]), 
  query(Url).	

query(Url)->
{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
      httpc:request(get, {Url, []}, [], []),
      Response=parse_xml(Body),
      {rsp,[{stat,"ok"}],RespBody}=Response,
      [_|[Photos|_]]=RespBody,
      {photos,[{page, Page},{pages, Pages},{perpage, PerPage}, {total, Total}],PhotoRefs}=Photos,
      Refs=lists:map(fun({_,Ref,_})->Ref end, lists:filter(fun(X)-> case X of "\n\t" -> false; "\n" -> false; _ -> true end end, PhotoRefs)),
      {Page, Pages, PerPage, Total, Refs}.

search_photos(UserId, ApiKey, SearchTerm, License)->
     Url=lists:flatten(["https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=", ApiKey,  "&text=", SearchTerm, "&license=", License, "&format=rest"]),
     query(Url).

parse_xml(Message)->
    {Xml, _}=xmerl_scan:string(Message),
    xmerl_lib:simplify_element(Xml).