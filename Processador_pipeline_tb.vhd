library ieee;
use ieee.numeric_bit.all;
use std.textio.all;

entity Processador_pipeline_tb is
end entity Processador_pipeline_tb;

architecture tb of Processador_pipeline_tb is
	component TinyRiscV is
	  generic
	  (		
		  --Control Unit
		  ControlWordSizeIdEx: natural := 11; --RegDst(1)| AluOP(5)| AluSrc(1)| Branch(1)| MemReadWrite(1)| MemToReg(1)|RegWrite(1)
		  ControlWordSizeExMem: natural := 4; --Branch(1)|MemReadWrite(1)|MemToReg(1)|RegWrite(1)
		  ControlWordSizeMemWb: natural := 2; --MemToReg(1)|RegWrite(1)
		  
		  --Datapath
		  addressSize : natural := 16;
		  wordSize : natural := 32;
		  
		  wordSizeIfId: natural := 64;
		  wordSizeIdEx: natural := 143;
		  wordSizeExMem: natural := 70;
		  wordSizeMemWb: natural := 69;
		  
		  IdControlMux: natural:=11;
		  ExControlMux: natural:=2;

		  datFileName : string := "rom.dat"
	  );
	end component;
		
	constant in_ControlWordSizeIdEx: integer := 11;
	constant in_ControlWordSizeExMem: integer := 4;
	constant in_ControlWordSizeMemWb: integer := 2;
	
	constant in_addressSize: integer := 16;
	constant in_wordSize: integer := 32;
	
	constant in_wordSizeIfId: integer := 64;
	constant in_wordSizeIdEx: integer := 143;
	constant in_wordSizeExMem: integer := 70;
	constant in_wordSizeMemWb: integer := 69;
	
	constant in_IdControlMux: integer := 11;
	constant in_ExControlMux: integer := 2;

	constant in_datFileName: string := "rom.dat";
	
begin
	DUT: TinyRiscV
		generic map(in_ControlWordSizeIdEx,in_ControlWordSizeExMem,in_ControlWordSizeMemWb,in_addressSize,in_wordSize,
					in_wordSizeIfId,in_wordSizeIdEx,in_wordSizeExMem,in_wordSizeMemWb,in_IdControlMux,in_ExControlMux,
					in_datFileName);
	
	stimulus: process is
	begin
	assert false report "simulation start" severity note;
		
	wait for 7100 ns;
	
	assert false report "Tudo certo" severity failure;
	wait;
	end process;

end tb;