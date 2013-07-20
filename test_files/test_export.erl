% Single line
-export([foo/1, bar/2]).

% Multiple lines
-export([foo/1, foo/2, foo/3, foo/4, foo/5, foo/6, foo/7, foo/8, foo/9, foo/10, foo/11]).

% Forced newline
-export([foo/1,
         foo/2]).

% Forced multiple new lines
-export([foo/1,


         foo/3]).

% With comments
-export([foo/1, % Does things


         % This does other things
         foo/4


         ]).

% With comment
-export([foo/1, % Does things
         foo/2]).

% Identity
-export([ foo/1 % Does things
        , foo/2
        ]).
