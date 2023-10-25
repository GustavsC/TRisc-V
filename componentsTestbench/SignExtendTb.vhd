library ieee;
use ieee.numeric_bit.all;

entity SignExtendTb is
end entity;

architecture tbarch of SignExtendTb is
	
	component signExtend is
	port(
		i:in bit_vector(15 downto 0); --input
		o:out bit_vector(31 downto 0) --output
	);
	end component;
	
  signal i,o: bit_vector(31 downto 0);
 
  begin

    teste: signExtend port map(i,o);

    Tbprocess: process is
        begin
            assert false report "simulation start" severity note;
			
            i<="0000001000000000";
			
            wait for 3 ns;

			i<="1000001000000000";
            wait for 3 ns;
				
            i<="0000001010000000";
			
			wait for 3 ns;
			
			i<="1000111000000000";
			
			wait for 3 ns;

	clock_end:=true;
	assert false report "Tudo certo" severity note;
	wait;
	end process;
	
end architecture;