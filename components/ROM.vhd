library ieee;
use ieee.numeric_bit.all;
use std.textio.all;

entity ROM is 
   generic(
	addressSize : natural := 16;
    wordSize : natural := 32;
    datFileName : string := "rom.dat";
	Tread : time := 5 ns
  );

  port(
    addr : in bit_vector(addressSize-1 downto 0);
    data : out bit_vector(wordSize-1 downto 0)
  );
end ROM;

--}} End of automatically maintained section

architecture ROM of ROM is
type mem_tipo is array (0 to 2**addressSize-1) of bit_vector(wordSize-1 downto 0);
    signal mem : mem_tipo;

   begin

    process
    impure function initiate_rom(file_name : string) return mem_tipo is
      file text_file : text open read_mode is file_name;
      variable text_line : line;
      variable temp_bv : bit_vector(wordSize-1 downto 0);
      variable rom_content : mem_tipo;
    
      begin
        for i in 0 to 2**addressSize-1 loop
          readline(text_file, text_line);
          read(text_line, temp_bv);
          rom_content(i) := temp_bv;
        end loop;
  
      return rom_content;
    end function;

  begin
    mem <= initiate_rom(datFileName); 
  
    wait;
  end process;

  data <= mem(to_integer(unsigned(addr))) after Tread;
end ROM;