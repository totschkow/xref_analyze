-module(proba).

-export([function/0]).

function() ->
    function1(),
    function2().

function1() ->
    function3().

function2() ->
    function3().

function3() ->
    function4().

function4() ->
    proba2:function().

