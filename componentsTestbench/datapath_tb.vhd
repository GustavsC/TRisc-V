library ieee;
use ieee.numeric_bit.all;
use std.textio.all;

entity datapath_tb is
end entity datapath_tb;

architecture tb of datapath_tb is
	shared variable clock_end: boolean:= false;
	
	component datapath is
		generic(
		  addressSize : natural := 16;
		  wordSize : natural := 32;
		  wordSizeIfId: natural := 64;
		  wordSizeIdEx: natural := 143;
		  wordSizeExMem: natural := 69;
		  wordSizeMemWb: natural := 69;
		  
		  IdControlMux: natural:=11;
		  ExControlMux: natural:=2;

		  datFileName : string := "rom.dat"
		);
		port(
		   --Control Signals
		   RegDst: in bit;
		   AluOP: in bit_vector(4 downto 0);
		   ALUSrc: in bit;
		   Branch: in bit;
		   MemReadWrite: in bit;
		   MemToReg: in bit;
		   RegWrite: in bit;
		   
		   --clock
		   clock: in bit;
		   
		   --Registers Enable
		   PCWrite: in bit;
		   IfIdWrite: in bit;
		   
		   --Flush Signals
		   IfFlush: in bit_vector(6 downto 0);
		   ExFlush: in bit;
		   IdFlush: in bit;
		   
		   --Forward Mux
		   ForwardA: in bit_vector(1 downto 0);
		   ForwardB: in bit_vector(1 downto 0);
		   
		   --Pipeline Control Signals
		   ControlSignals: in bit_vector(10 downto 0);
		   
		   --Instruction
		   Instruction: out bit_vector(31 downto 0);
		   zero: out bit;
		   
		   --Pipeline Control Registers
		   outDataControl_IdEx: in bit_vector(3 downto 0);
		   inDataControl_IdEx: out bit_vector(10 downto 0);
		   inDataControl_ExMem: out bit_vector(3 downto 0);
		   
		   --Hazard Detection 
		   Hazard_Ex_Rt: out bit_vector(4 downto 0);
		   Hazard_Id_Rt: out bit_vector(4 downto 0);
		   Hazard_Id_Rs: out bit_vector(4 downto 0);
			
		   --Forward Unit
		   Forward_Ex_Rs,Forward_Ex_Rt,Forward_Wb_Register,Forward_Mem_Register: out bit_vector(4 downto 0)
		   
		);
	end component;
	
	constant in_addressSize: integer := 16;
	constant in_wordSize: integer := 32;
	
	constant in_wordSizeIfId: integer := 64;
	constant in_wordSizeIdEx: integer := 143;
	constant in_wordSizeExMem: integer := 69;
	constant in_wordSizeMemWb: integer := 69;
	
	constant in_IdControlMux: integer := 11;
	constant in_ExControlMux: integer := 2;

	constant in_datFileName: string := "rom.dat";
	
	
		  --Control Signals
	 signal S_RegDst: bit;
	 signal S_AluOP:  bit_vector(4 downto 0);
	 signal S_ALUSrc: bit;
	 signal S_Branch: bit;
	 signal S_MemReadWrite: bit;
	 signal S_MemToReg: bit;
	 signal S_RegWrite: bit;
	  
	  --Clock
	 signal S_clock: bit;
	  
	  --Registers Enable
	 signal S_PCWrite: bit;
	 signal S_IfIdWrite: bit;
	  
	  --Flushes
	 signal S_IfFlush: bit_vector(6 downto 0);
	 signal S_ExFlush: bit; 
	 signal S_IdFlush: bit;
		
	  --Forward Mux
	 signal S_ForwardA: bit_vector(1 downto 0);
	 signal S_ForwardB: bit_vector(1 downto 0);
	  
	  --Pipeline Control Signals
	 signal S_ControlSignals: bit_vector(10 downto 0);
	  
	  --Instruction
	 signal S_Instruction: bit_vector(31 downto 0);
	 signal S_zero: bit;
	  
	  --Pipeline Control Registers
	 signal S_MuxOutDataControl_IdEx: bit_vector(3 downto 0);
	 signal S_inDataControl_IdEx: bit_vector(10 downto 0);
	 signal S_inDataControl_ExMem: bit_vector(3 downto 0);
	  
	  --Hazard Detection registers
	 signal S_Ex_Rt: bit_vector(4 downto 0);
	 signal S_Id_Rt: bit_vector(4 downto 0);
	 signal S_Id_Rs: bit_vector(4 downto 0);
		
	  --Forward Unit
	 signal S_Forward_Ex_Rs: bit_vector(4 downto 0);
	 signal S_Forward_Ex_Rt: bit_vector(4 downto 0);
	 signal S_Forward_Wb_Register: bit_vector(4 downto 0);
	 signal S_Forward_Mem_Register: bit_vector(4 downto 0);
	 
begin
	DUT: datapath
		generic map(in_addressSize,in_wordSize,
					in_wordSizeIfId,in_wordSizeIdEx,in_wordSizeExMem,in_wordSizeMemWb,in_IdControlMux,in_ExControlMux,
					in_datFileName)
		port map(S_RegDst,S_AluOP,S_ALUSrc,S_Branch,S_MemReadWrite,S_MemToReg,S_RegWrite,S_clock,S_PCWrite,S_IfIdWrite,S_IfFlush,S_ExFlush,S_IdFlush,S_ForwardA,
					S_ForwardB,S_ControlSignals,S_Instruction,S_zero,S_MuxOutDataControl_IdEx,S_inDataControl_IdEx,S_inDataControl_ExMem,S_Ex_Rt,
					S_Id_Rt,S_Id_Rs,S_Forward_Ex_Rs,S_Forward_Ex_Rt,S_Forward_Wb_Register,S_Forward_Mem_Register);
	
	clk: process is
		begin
			S_clock <= '0';
			wait for 25 ns;
			S_clock <= '1';
			wait for 25 ns;
			if(clock_end=true) then
				wait;
			end if;
	end process clk; 
	
	
	stimulus: process is
	begin
	assert false report "simulation start" severity note;
		
	S_RegDst<='1';
	S_AluOP<="00001";
	S_PCWrite<='1';
	wait for 2100 ns;
	
	clock_end:=true;
	assert false report "Tudo certo" severity failure;
	wait;
	end process;

end tb;