-module(bit_shift).
-export([extract_bits/1, test/0]).

extract_bits(Int) ->
    Lower16 = Int band 16#FFFF,          % Mask to get bits 0-15
    Upper16 = (Int bsr 16) band 16#FFFF, % Shift right 16 bits then mask to get bits 16-31
    {Lower16, Upper16}.
test() ->
{65535, 65535} = extract_bits(4294967295),
{65535, 255} = extract_bits(16777215),
{0, 0} = extract_bits(0),
{1, 0} = extract_bits(1),
{0, 1} = extract_bits(65536),
ok.