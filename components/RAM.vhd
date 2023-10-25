library ieee;
use ieee.numeric_bit.all;

entity mem_RAM is 
  generic(
    addressSize : natural := 16;
    wordSize : natural := 32;
	Tread : time := 5 ns;
	Twrite : time := 5 ns
  );

  port(
    ck, wr : in bit;
    addr : in bit_vector(addressSize-1 downto 0);
    data_i : in bit_vector(wordSize-1 downto 0);
    data_o : out bit_vector(wordSize-1 downto 0)
  );
end mem_RAM;

architecture arch_RAM of mem_RAM is
    type mem_tipo is array (0 to 2**addressSize-1) of bit_vector(wordSize-1 downto 0);
    signal mem : mem_tipo;

   begin

    process(ck)  
    begin
      if ck = '1' then 
        if wr = '1' then
          mem(to_integer(unsigned(addr))) <= data_i after Twrite;
        end if;    
      end if;

    end process;

    process(addr, wr)
    begin
      if wr = '0' then
        data_o <= mem(to_integer(unsigned(addr))) after Tread;
      end if;

    end process;

end arch_RAM;
