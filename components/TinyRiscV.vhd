library ieee;
use ieee.numeric_bit.all;
use std.textio.all;

entity TinyRiscV is
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
	  wordSizeExMem: natural := 69;
	  wordSizeMemWb: natural := 69;
	  
	  IdControlMux: natural:=11;
	  ExControlMux: natural:=2;

	  datFileName : string := "rom.dat"
  );
end entity;

architecture Processor of TinyRiscV is
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
	
	component ControlUnit is
	generic(
	  ControlWordSizeIdEx: natural := 11; --RegDst(1)| AluOP(5)| AluSrc(1)| Branch(1)| MemReadWrite(1)| MemToReg(1)|RegWrite(1)
	  ControlWordSizeExMem: natural := 4; --Branch(1)|MemReadWrite(1)|MemToReg(1)|RegWrite(1)
	  ControlWordSizeMemWb: natural := 2  --MemToReg(1)|RegWrite(1)
	);

	port(
	  --Control Signals
	  RegDst: out bit;
	  AluOP: out bit_vector(4 downto 0);
	  ALUSrc: out bit;
	  Branch: out bit;
	  MemReadWrite: out bit;
	  MemToReg: out bit;
	  RegWrite: out bit;
	  
	  --Clock
	  clock: out bit;
	  
	  --Registers Enable
	  PCWrite: out bit;
	  IfIdWrite: out bit;
	  
	  --Flushes
	  IfFlush: out bit_vector(6 downto 0);
	  ExFlush: out bit; 
	  IdFlush: out bit;
		
	  --Forward Mux
	  ForwardA:out bit_vector(1 downto 0);
	  ForwardB:out bit_vector(1 downto 0);
	  
	  --Pipeline Control Signals
	  ControlSignals: out bit_vector(10 downto 0);
	  
	  --Instruction
	  Instruction: in bit_vector(31 downto 0);
	  zero: in bit;
	  
	  --Pipeline Control Registers
	  MuxOutDataControl_IdEx: out bit_vector(3 downto 0);
	  inDataControl_IdEx: in bit_vector(10 downto 0);
	  inDataControl_ExMem: in bit_vector(3 downto 0);
	  
	  --Hazard Detection registers
	  Ex_Rt: in bit_vector(4 downto 0);
	  Id_Rt: in bit_vector(4 downto 0);
	  Id_Rs: in bit_vector(4 downto 0);
		
	  --Forward Unit
	  Forward_Ex_Rs: in bit_vector(4 downto 0);
	  Forward_Ex_Rt: in bit_vector(4 downto 0);
	  Forward_Wb_Register: in bit_vector(4 downto 0);
	  Forward_Mem_Register: in bit_vector(4 downto 0)

	);
	end component;
	
	
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
	Path: datapath
		generic map(addressSize,wordSize,wordSizeIfId,wordSizeIdEx,wordSizeExMem,wordSizeMemWb,IdControlMux,ExControlMux,datFileName)
		port map(S_RegDst,S_AluOP,S_ALUSrc,S_Branch,S_MemReadWrite,S_MemToReg,S_RegWrite,S_clock,S_PCWrite,S_IfIdWrite,S_IfFlush,S_ExFlush,S_IdFlush,S_ForwardA,
					S_ForwardB,S_ControlSignals,S_Instruction,S_zero,S_MuxOutDataControl_IdEx,S_inDataControl_IdEx,S_inDataControl_ExMem,S_Ex_Rt,
					S_Id_Rt,S_Id_Rs,S_Forward_Ex_Rs,S_Forward_Ex_Rt,S_Forward_Wb_Register,S_Forward_Mem_Register);
	
	Control: ControlUnit
		generic map(ControlWordSizeIdEx,ControlWordSizeExMem,ControlWordSizeMemWb)
		port map(S_RegDst,S_AluOP,S_ALUSrc,S_Branch,S_MemReadWrite,S_MemToReg,S_RegWrite,S_clock,S_PCWrite,S_IfIdWrite,S_IfFlush,S_ExFlush,S_IdFlush,S_ForwardA,
					S_ForwardB,S_ControlSignals,S_Instruction,S_zero,S_MuxOutDataControl_IdEx,S_inDataControl_IdEx,S_inDataControl_ExMem,S_Ex_Rt,
					S_Id_Rt,S_Id_Rs,S_Forward_Ex_Rs,S_Forward_Ex_Rt,S_Forward_Wb_Register,S_Forward_Mem_Register);

end architecture;