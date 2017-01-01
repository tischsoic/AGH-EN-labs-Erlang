-module(temp_conversion).
-export([conv_temp/2]).


conv_fah2cel(T) -> (T - 32) * 5 / 9.
conv_cel2fah(T) -> T * 9 / 5 + 32.

conv_kel2cel(T) -> T - 273.15.
conv_cel2kel(T) -> T + 273.15.

conv_ran2cel(T) -> (T - 491.57) * 5 / 9.
conv_cel2ran(T) -> (T + 273.15) * 9 / 5.

conv_del2cel(T) -> 100 - T * 2 / 3.
conv_cel2del(T) -> (100 - T) * 3 / 2.

conv_new2cel(T) -> T * 100 / 33.
conv_cel2new(T) -> T * 33 / 100.

conv_temp(Temp, {cel, cel}) -> Temp;

conv_temp(Temp, {cel, fah}) -> conv_cel2fah(Temp);
conv_temp(Temp, {fah, cel}) -> conv_fah2cel(Temp);

conv_temp(Temp, {cel, kel}) -> conv_cel2kel(Temp);
conv_temp(Temp, {kel, cel}) -> conv_kel2cel(Temp);

conv_temp(Temp, {cel, ran}) -> conv_cel2ran(Temp);
conv_temp(Temp, {ran, cel}) -> conv_ran2cel(Temp);

conv_temp(Temp, {cel, del}) -> conv_cel2del(Temp);
conv_temp(Temp, {del, cel}) -> conv_del2cel(Temp);

conv_temp(Temp, {cel, new}) -> conv_cel2new(Temp);
conv_temp(Temp, {new, cel}) -> conv_new2cel(Temp);

conv_temp(Temp, {FromUnit, ToUnit}) -> 
	T2 = conv_temp(Temp, {FromUnit, cel}), 
	conv_temp(T2, {cel, ToUnit}).

