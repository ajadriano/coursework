-module(temperature).
-export([toCelsius/1]).
-export([toFarenheit/1]).
-export([controller/0]).
-export([temperatureConverterActor/1]).
-export([displayActor/0]).

toCelsius(Farenheit) -> ((Farenheit - 32) * 5) / 9.

toFarenheit(Celsius) -> ((Celsius * 9) / 5) + 32.

controller() ->
	receive
	new -> 
		io:format("Creating temperature converter actor and display actor.~n"),
		register(displayActor,spawn_link(temperature, displayActor, [])),
		register(temperatureConverterActor,spawn_link(temperature, temperatureConverterActor, [displayActor])),
		controller();
	{convertToCelsius, Celsius} ->  temperatureConverterActor ! {convertToCelsius, Celsius},controller();
	{convertToFarenheit, Farenheit} -> temperatureConverterActor ! {convertToFarenheit, Farenheit},controller();
	_ ->
		io:format("Unexpected message received.~n"),
		controller()
	end.

temperatureConverterActor(DisplayActor) ->
	receive
	{convertToCelsius, Celsius} ->  DisplayActor ! {temperature, toCelsius(Celsius)},temperatureConverterActor(DisplayActor);
	{convertToFarenheit, Farenheit} -> DisplayActor ! {temperature, toFarenheit(Farenheit)},temperatureConverterActor(DisplayActor);
	_ -> io:format("Unexpected message received.~n"),temperatureConverterActor(DisplayActor)
	end.

displayActor() ->
	receive
	{temperature, Temperature} -> io:format("~p~n", [Temperature]), displayActor();
	_ -> io:format("Unexpected message received.~n"),displayActor()
	end.
