--------------------------HazardDetection---------------------------------------------
library ieee;
use ieee.numeric_bit.all;

entity HazardDetection is 
port(
	instruction: in bit_vector(31 downto 0);
	Ex_Rt: in bit_vector(4 downto 0);
	Id_Rt: in bit_vector(4 downto 0);
	Id_Rs: in bit_vector(4 downto 0);
	Ex_MemRead: in bit;
	IfIdWrite: out bit;
	PCWrite: out bit;
	IdFlush: out bit
);
end entity HazardDetection;

architecture hazard of HazardDetection is
begin
	process(instruction,Ex_Rt,Ex_MemRead)
	begin
		if(Ex_MemRead = '1' and (Ex_Rt = Id_Rs or Ex_Rt = Id_Rt)) then
			PCWrite<= '0';
			IfIdWrite<= '0';
			IdFlush<='0';
		else
			PCWrite<= '1';
			IfIdWrite<= '1';
			IdFlush<='1';
		end if;
	end process;
end architecture;

----------------------------------------------------------------ForwardingUnit-------------------------------------------------
library ieee;
use ieee.numeric_bit.all;

entity ForwardingUnit is
port(
	MemRegWrite: in bit;
	WbRegWrite: in bit;
	Forward_Ex_Rs: in bit_vector(4 downto 0);
	Forward_Ex_Rt: in bit_vector(4 downto 0);
	Forward_Wb_Register: in bit_vector(4 downto 0);
	Forward_Mem_Register: in bit_vector(4 downto 0);
	ForwardA: out bit_vector(1 downto 0);
	ForwardB: out bit_vector(1 downto 0)
);
end entity ForwardingUnit;

architecture forward of ForwardingUnit is
begin
	process(Forward_Ex_Rs,Forward_Ex_Rt,Forward_Wb_Register,Forward_Mem_Register)
	begin
		if(MemRegWrite = '1' and Forward_Mem_Register /= "00000" and Forward_Mem_Register = Forward_Ex_Rs) then --Ex Hazard
			ForwardA <= "10";
			if(Forward_Mem_Register = Forward_Ex_Rt) then
				ForwardB<="10";
			else 
				ForwardB<="00";
			end if;
		else
			ForwardA<="00";
			ForwardB<="00";
		end if;

		if(WbRegWrite = '1' and Forward_Wb_Register /="00000" and (not(MemRegWrite='1' and Forward_Mem_Register /= "00000")) and Forward_Mem_Register /= Forward_Ex_Rs and Forward_Wb_Register = Forward_Ex_Rs) then --Mem Hazard
			ForwardA<="01";
			if(Forward_Wb_Register = Forward_Ex_Rt) then
				ForwardB<="01";
			else 
				ForwardB<="00";
			end if;
		else
			ForwardA<="00";
			ForwardB<="00";
		end if;
	end process;
end architecture;

--------------------------------------------------------------------State Machine-----------------------------------------------------------------
library ieee;
use ieee.numeric_bit.all;

entity ControlStateMachine is
  generic(
	  ClockCycle : time := 50 ns --Arruma o ciclo do relogio dps, não lembro quanto era
  );

port(
	Instruction: in bit_vector(31 downto 0);
	zero: in bit;
	clock: out bit;
	IfFlush: out bit_vector(6 downto 0);
	ExFlush: out bit; 
	IdFlush: out bit;
	RegDst: out bit;
	AluOP: out bit_vector(4 downto 0);
	ALUSrc: out bit;
	Branch: out bit;
	MemReadWrite: out bit;
	MemToReg: out bit;
	RegWrite: out bit
);
end entity;
	
architecture arch of ControlStateMachine is

    signal EstadoAtual, ProximoEstado: bit_vector(4 downto 0);
	signal clock_in: bit;

begin

    Relogio: process
	begin
		clock <= '0';
        wait for ClockCycle/2;
        clock <= '1';
        wait for ClockCycle/2;
	end process;
    
	Estados: process
    begin
        EstadoAtual <= ProximoEstado;
        wait for ClockCycle;
    end process;
	
	 
	ExFlush<= '0' when EstadoAtual = "00000" else '1';
	IdFlush<= '0' when EstadoAtual = "00000" else '1';
	IfFlush<= "0000000" when EstadoAtual = "00000";
	
	RegDst<='1' when (EstadoAtual = "01000" or EstadoAtual = "01001" or EstadoAtual = "01010" or EstadoAtual = "01011" or EstadoAtual = "01100" or
				EstadoAtual = "01101") else '0';
	
	
	AluOP <= "00000" when EstadoAtual="00101" or EstadoAtual = "01101" else --ANDI|AND
          "00001" when EstadoAtual = "00110" or EstadoAtual = "01100" else   --ORI|OR
		  "00010" when (EstadoAtual="00100" or EstadoAtual="01000" or EstadoAtual="01110" or EstadoAtual="01111") else -- A+B
		  "01010" when EstadoAtual="01001" else -- A-B
		  "00100" when (EstadoAtual="00001") else -- shift left
		  "00101" when (EstadoAtual="00111") else -- shift right 
		  "00110" when (EstadoAtual="00010") else -- shift right Ari
          
		  "01011" when (EstadoAtual="00011" or EstadoAtual="10000" or EstadoAtual="10001" or EstadoAtual="10010"); -- A < B Slt
        --"XXXX" when (EstadoAtual="01011") else -- shift right
	
	ALUSrc <= '1' when (EstadoAtual="01110" or EstadoAtual="01111") else '0';
      
		
    Branch <= '1' when (EstadoAtual="10000" or EstadoAtual="10001" or EstadoAtual="10010")
				  else '0';

	MemReadWrite <= '1' when (EstadoAtual="01110") else '0';


	MemtoReg <= '1' when EstadoAtual="01111" else '0';
	
	
    RegWrite <= '1' when (EstadoAtual = "01000" or EstadoAtual = "01001" or EstadoAtual = "01010" or EstadoAtual = "01011" or EstadoAtual = "01100" or
				EstadoAtual = "01101" or EstadoAtual="01111") else '0'; 
   
	
	
	ProximoEstado<=
    "00001" when EstadoAtual="00000"  and Instruction(6 downto 0)="0010011" and Instruction(14 downto 12)="001" else --SLLI (Shift Left Logical Immeadiate) 
    "00010" when EstadoAtual="00000"  and Instruction(6 downto 0)="0010011" and Instruction(14 downto 12)="101" and Instruction(30)='1' else --SRAI (Shift Right Arithmetic Immediate)
    "00011" when EstadoAtual="00000"  and Instruction(6 downto 0)="0010011" and Instruction(14 downto 12)="010" else --SLTI (Set less than Immediate)
    "00100" when EstadoAtual="00000"  and Instruction(6 downto 0)="0010011" and Instruction(14 downto 12)="000" else --ADDI 
	"00101" when EstadoAtual="00000"  and Instruction(6 downto 0)="0010011" and Instruction(14 downto 12)="111" else --ANDI 
	"00110" when EstadoAtual="00000"  and Instruction(6 downto 0)="0010011" and Instruction(14 downto 12)="110" else --ORI 
	"00111" when EstadoAtual="00000"  and Instruction(6 downto 0)="0010011" and Instruction(14 downto 12)="101" and Instruction(30)='0' else --SRLI (Shift Right logical Immediate)
	
	
    "01000" when EstadoAtual="00000" and Instruction(6 downto 0)="0110011" and Instruction(14 downto 12)="000" and Instruction(30)='0'and Instruction(25)='0' else  --Add
	"01001" when EstadoAtual="00000" and Instruction(6 downto 0)="0110011" and Instruction(14 downto 12)="000" and Instruction(30)='1' and Instruction(25)='0'else  --SUB
	"01010" when EstadoAtual="00000" and Instruction(6 downto 0)="0110011" and Instruction(14 downto 12)="000" and Instruction(30)='0' and Instruction(25)='1' else --MUL
	"01011" when EstadoAtual="00000" and Instruction(6 downto 0)="0110011" and Instruction(14 downto 12)="010"  else                                                --SLT (Set Less Than)
	"01100" when EstadoAtual="00000" and Instruction(6 downto 0)="0110011" and Instruction(14 downto 12)="110" and Instruction(30)='0' and Instruction(25)='0'else  --OR
	"01101" when EstadoAtual="00000" and Instruction(6 downto 0)="0110011" and Instruction(14 downto 12)="111" and Instruction(30)='0' and Instruction(25)='0'else  --AND
	
	"01110" when EstadoAtual="00000" and Instruction(6 downto 0)="0100011" else --Store
	
	"01111" when EstadoAtual="00000" and Instruction(6 downto 0)="0000011" else --Load(U)
	
	"10000" when EstadoAtual="00000" and Instruction(6 downto 0)="1100011" and Instruction(14 downto 12)="000" else --Branch Equal(BEQ)
	"10001" when EstadoAtual="00000" and Instruction(6 downto 0)="1100011" and Instruction(14 downto 12)="001" else --Branch Not Equal (BNE)
	"10010" when EstadoAtual="00000" and Instruction(6 downto 0)="1100011" and Instruction(14 downto 12)="100" else --Branch Less Than (BLT)
	
	
	"10011" when EstadoAtual="00000" and (Instruction(6 downto 0)="1101111") else --JAL
	"10100" when EstadoAtual="00000" and (Instruction(6 downto 0)="1100111") else --JALR
   
    "00000"; -- Estado 0 (de busca)
	
end architecture;

library ieee;
use ieee.numeric_bit.all;

entity ControlUnit is
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
end entity;

architecture arch_ControlUnit of ControlUnit is

	component regXbits is
	  generic(
		wordSize : natural := 32
	  );
	 port (
		clock: in bit;
		ce: in bit;
		reset: in bit;
		Din: in bit_vector(wordSize-1 downto 0);
		Dout: out bit_vector(wordSize-1 downto 0)
	  ) ;
	end component;
	
	component ForwardingUnit is
	port(
		MemRegWrite: in bit;
		WbRegWrite: in bit;
		Forward_Ex_Rs: in bit_vector(4 downto 0);
		Forward_Ex_Rt: in bit_vector(4 downto 0);
		Forward_Wb_Register: in bit_vector(4 downto 0);
		Forward_Mem_Register: in bit_vector(4 downto 0);
		ForwardA: out bit_vector(1 downto 0);
		ForwardB: out bit_vector(1 downto 0)
	);
	end component;
	
	component ControlStateMachine is
	  generic(
		  ClockCycle : time := 50 ns --Arruma o ciclo do relogio dps, não lembro quanto era
	  );

	port(
		Instruction: in bit_vector(31 downto 0);
		zero: in bit;
		clock: out bit;
		IfFlush: out bit_vector(6 downto 0);
		ExFlush: out bit; 
		IdFlush: out bit;
		RegDst: out bit;
		AluOP: out bit_vector(4 downto 0);
		ALUSrc: out bit;
		Branch: out bit;
		MemReadWrite: out bit;
		MemToReg: out bit;
		RegWrite: out bit
	);
	end component;
	
	component HazardDetection is 
	port(
		instruction: in bit_vector(31 downto 0);
		Ex_Rt: in bit_vector(4 downto 0);
		Id_Rt: in bit_vector(4 downto 0);
		Id_Rs: in bit_vector(4 downto 0);
		Ex_MemRead: in bit;
		IfIdWrite: out bit;
		PCWrite: out bit;
		IdFlush: out bit
	);
	end component;

	signal inDataControl_MemWb: bit_vector(1 downto 0);
	
	signal outDataControl_IdEx: bit_vector(10 downto 0);
	
	signal outDataControl_ExMem: bit_vector(3 downto 0);
	signal outDataControl_MemWb: bit_vector(1 downto 0);
	
	signal S_RegDst: bit;
	signal S_AluOP: bit_vector(4 downto 0);
	signal S_ALUSrc: bit;
	signal S_Branch: bit;
	signal S_MemReadWrite: bit;
	signal S_MemToReg: bit;
	signal S_RegWrite: bit;
	
	signal S_clock:bit;
	signal MemRegWrite,WbRegWrite:bit;
	
	signal Ex_MemRead: bit;
	
	signal IdFlush_Hazard,IdFlush_State: bit;

begin
		
	clock<=S_clock;
	ControlSignals<=S_RegDst & S_AluOP & S_ALUSrc & S_Branch & S_MemReadWrite & S_MemToReg & S_RegWrite;
	
	MuxOutDataControl_IdEx<= outDataControl_IdEx(3 downto 0);
	
	Ex_MemRead<=outDataControl_IdEx(2);
	
	MemRegWrite<=outDataControl_ExMem(0);
	WbRegWrite<=outDataControl_MemWb(0);
	
	inDataControl_MemWb<=outDataControl_ExMem(1 downto 0);
	
	IdFlush<= IdFlush_Hazard or IdFlush_State;
	
	--Control Signals to datapath
	RegDst<= outDataControl_IdEx(10);
	AluOP<= outDataControl_IdEx(9 downto 5);
	ALUSrc<= outDataControl_IdEx(4);
	
	Branch<=outDataControl_ExMem(3);
	MemReadWrite<=outDataControl_ExMem(2);
	
	MemToReg<=outDataControl_MemWb(1);
	RegWrite<=outDataControl_MemWb(0);
	
	
	
	Control_IdEx: regXbits
		generic map(ControlWordSizeIdEx)
		port map(S_clock,'1','0',inDataControl_IdEx,outDataControl_IdEx);
		
	Control_ExMem: regXbits
		generic map(ControlWordSizeExMem)
		port map(S_clock,'1','0',inDataControl_ExMem,outDataControl_ExMem);
		
	
	Control_MemWb:regXbits 
		generic map(ControlWordSizeMemWb)
		port map(S_clock,'1','0',inDataControl_MemWb,outDataControl_MemWb);
	
	
	StateMachine: ControlStateMachine
		port map(Instruction,zero,S_clock,IfFlush,ExFlush,IdFlush_State,S_RegDst,S_AluOP,S_ALUSrc,S_Branch,S_MemReadWrite,S_MemToReg,S_RegWrite);
	
	Forwarding: ForwardingUnit
		port map(MemRegWrite,WbRegWrite,Forward_Ex_Rs,Forward_Ex_Rt,Forward_Wb_Register,Forward_Mem_Register,ForwardA,ForwardB);

	HazardDet: HazardDetection
		port map(Instruction,Ex_Rt,Id_Rt,Id_Rs,Ex_MemRead,IfIdWrite,PCWrite,IdFlush_Hazard);

end architecture;