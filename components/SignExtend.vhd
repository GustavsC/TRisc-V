library ieee;
use ieee.numeric_bit.all;

entity signExtend is
	port(
		i:in bit_vector(15 downto 0); --input
		o:out bit_vector(31 downto 0) --output
	);
end signExtend;

architecture sinal of signExtend is
begin
	process(i)
	begin
		if(i(15)='1') then
			o<="1111111111111111"& i;
		else
			o<="0000000000000000"& i;
		end if;
	end process;
		
end architecture;