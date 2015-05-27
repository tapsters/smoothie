-ifndef(SM_HRL).
-define(SM_HRL, 1).

-record(sm_response, {
  status=200, 
  headers=[], 
  body= <<>>, 
  cookies=[]
}).

-record(sm_cookie, {
  name, 
  value, 
  max_age,  %% optional
  domain,   %% optional
  path,     %% optional
  secure,   %% optional
  http_only %% optional
}).

-endif.