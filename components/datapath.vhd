library ieee;
use ieee.std_logic_1164.all;

library ieee;
use ieee.numeric_bit.all;

entity regXbits is
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
end regXbits;

architecture arch of regXbits is
	signal Tsetup: bit;
	signal Din_anterior: bit_vector(wordSize-1 downto 0);
	signal zero: bit_vector(wordSize-1 downto 0);
begin
	process(Din,clock)
	begin
		if(Din = Din_anterior) then
			Tsetup<='0';
		else
			Tsetup<='1' after 0.25 ns;
		end if;
	
	end process;
	
	
	process(clock) 
		begin
			if(reset='1') then
				Dout<=zero after 1.25 ns;
			elsif (((ce='1') and (rising_edge(clock))) and Tsetup = '1') then 
				Dout <= Din after 1.25 ns;
				Din_anterior<=Din;
			end if;
	end process;

end architecture ; -- arch

--------------------------------------------------------Instruction Memory--------------------------------------------------
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

-----------------------------------------------RAM de Dados----------------------------------------
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

-------------------------------------------------------------Regfile----------------------------------------

library ieee;
use ieee.numeric_bit.all;

entity banco_registradores is
	port(
		clock: in bit;
		reset: in bit;
		regWrite: in bit;
		read_register_1, read_register_2, write_register:in bit_vector(4 downto 0);
		write_data: in bit_vector(31 downto 0);
		read_data_1,read_data_2: out bit_vector(31 downto 0)
	);
end banco_registradores;

architecture registrador_grande of banco_registradores is
	type memoria is array (0 to 31) of bit_vector(31 downto 0);
signal mem : memoria;
begin
	read_data_1<=mem(to_integer(unsigned(read_register_1))) after 5 ns;
	read_data_2<=mem(to_integer(unsigned(read_register_2))) after 5 ns;
process(clock)
	variable teste: unsigned(4 downto 0):= (others=>'1');
begin
	if((rising_edge(clock) and regWrite='1') and unsigned(write_register)/=teste) then
		mem(to_integer(unsigned(write_register)))<=write_data after 5 ns;
	end if;
	if(reset='1') then
		for i in 0 to 30 loop
			mem(i)<=(others=>'0');
		end loop;
	end if;
end process;
end architecture;

-----------------------------------------------------------------------------------------------------
library ieee;
use ieee.numeric_bit.all;

entity multiplexador2 is
	generic(
		wordSize : natural := 32
	);
	port(
			A: in bit_vector(wordSize-1 downto 0);
			B: in bit_vector(wordSize-1 downto 0);
			sel: in bit;
			saida: out bit_vector(wordSize-1 downto 0)
	);
end entity;

architecture mux of multiplexador2 is
	signal sel_antigo: bit;
begin
	process(A,B,sel)
	begin
		if(sel /= sel_antigo) then		
			sel_antigo<=sel;
			if(sel = '0') then
				saida<=A after 0.5 ns;
			else
				saida<=B after 0.5 ns;
			end if;
		else
			if(sel = '0') then
				saida<=A after 0.25 ns;
			else
				saida<=B after 0.25 ns;
			end if;
		end if;
	end process;
end architecture;

------------------------------------------------------------------MUX 4--------------------------------------------------------
library ieee;
use ieee.numeric_bit.all;

entity multiplexador4 is
	generic(
		wordSize: natural :=32
	);
	port(
			A: in bit_vector(wordSize-1 downto 0);
			B: in bit_vector(wordSize-1 downto 0);
			C: in bit_vector(wordSize-1 downto 0);
			D: in bit_vector(wordSize-1 downto 0);
			sel: in bit_vector(1 downto 0);
			saida: out bit_vector(wordSize-1 downto 0)
	);
end entity;

architecture mux of multiplexador4 is
	signal sel_antigo: bit_vector(1 downto 0);
begin
	process(A,B,C,D,sel)
	begin
		if(sel /= sel_antigo) then		
			sel_antigo<=sel;
			if(sel = "00") then
				saida<=A after 0.5 ns;
			elsif(sel = "01") then
				saida<=B after 0.5 ns;
			elsif(sel = "10") then
				saida<=C after 0.5 ns;
			else
				saida<=D after 0.5 ns;
			end if;
		else
			if(sel = "00") then
				saida<=A after 0.25 ns;
			elsif(sel = "01") then
				saida<=B after 0.25 ns;
			elsif(sel = "10") then
				saida<=C after 0.25 ns;
			else
				saida<=D after 0.25 ns;
			end if;
		end if;
	end process;
end architecture;

----------------------------------------------------------------Little ULA-----------------------------------------------------
library ieee;
use ieee.numeric_bit.all;

entity littleUla is
port(
	A: in bit_vector(31 downto 0);
	B: in bit_vector(31 downto 0);
	saida: out bit_vector(31 downto 0)
);
end entity;

architecture soma of littleUla is
begin
	process(A,B)
		variable A_in,B_in,saida_out: unsigned(31 downto 0);
	begin
		A_in:=unsigned(A);
		B_in:=unsigned(B);
		saida_out:=A_in+B_in;
		saida<=bit_vector(saida_out) after 1 ns;
	end process;
end architecture;

--------------------------------------------------------------Shift Left 2 -----------------------------------------------------
library ieee;
use ieee.numeric_bit.all;

entity shiftleft2 is
	port(
		entrada: in bit_vector(31 downto 0);
		saida: out bit_vector(31 downto 0)
	);
end entity;

architecture deslocando of shiftleft2 is
begin
	process(entrada)
		variable multiplicando: unsigned(31 downto 0);
	begin
		multiplicando:=unsigned(entrada);
		multiplicando:=multiplicando+multiplicando+multiplicando+multiplicando;
		saida<=bit_vector(multiplicando);
	
	end process;
end architecture;

----------------------------------------------------------IMM/SignExtend------------------------------------------------------
library ieee;
use ieee.numeric_bit.all;

entity SignExtend is
    port (
      Ri: in bit_vector(31 downto 0);
      Imm: out bit_vector(31 downto 0)
    );
end entity;

architecture arch of SignExtend is
    signal opcode: bit_vector(6 downto 0);

begin
    opcode<=Ri(6 downto 0); 

    with opcode select Imm <=
        Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)
        &Ri(31 downto 20) 
        when "0010011",  --Tipo I
        Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)
        &Ri(31 downto 25)&Ri(11 downto 7) 
        when "0100011",  --Tipo S
        Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)
        &Ri(31)&Ri(19 downto 12)&Ri(20)&Ri(30 downto 21)&'0'
        when "1101111", --Jal
        Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)
        &Ri(31 downto 20)
        when "0000011", --Tipo U
        Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)
        &Ri(31)&Ri(7)&Ri(30 downto 25)&Ri(11 downto 8)&'0'
        when "1100011", --Tipo B
        Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)&Ri(31)
        &Ri(31 downto 20)
        when "1100111", --Tipo Jalr
        "00000000000000000000000000000000" when others; 
  
end architecture ; -- arch

----------------------------------------------------------Decoder--------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity decode is
	port(
	instruction: in bit_vector(31 downto 0);
	imm: out bit_vector(20 downto 0);-- J
	opcode: out bit_vector(6 downto 0);
	rd: out bit_vector(4 downto 0);
	funct3: out bit_vector(2 downto 0);
	rs1: out bit_vector(4 downto 0);
	rs2: out bit_vector(4 downto 0);
	funct7: out bit_vector(6 downto 0);
	offset:	out bit_vector(11 downto 0); -- Outros
	base: out bit_vector(4 downto 0);
	shamt: out bit_vector (4 downto 0)
	);
end entity decode;

architecture dec of decode is
begin
	process(instruction)
	begin
		case instruction(6 downto 0) is
			when "0110011" => 
				case instruction(14 downto 12) is
					when "000"|"010"	=>	 --Add|SLT 		
						funct7<="0000000" after 0.5 ns;  --Instruction(31 downto 25)
						rs2<=instruction(24 downto 20) after 0.5 ns;
						rs1<=instruction(19 downto 15) after 0.5 ns;
						funct3<=instruction(14 downto 12) after 0.5 ns;
						rd<=instruction(11 downto 7) after 0.5 ns;
						opcode<= "0110011" after 0.5 ns;	  --instruction(6 downto 0);
					when others =>
						opcode <= "0000000" after 0.5 ns;
				end case;	
			
			when "0010011" => --ADDI|SLTI|SLLI|SRLI|SRAI
				case instruction (14 downto 12) is
					when "000"|"010"  =>   --ADDI|SLTI
						offset(11 downto 0)<= instruction(31 downto 20) after 0.5 ns;
						rs1<=instruction(19 downto 15) after 0.5 ns;
						funct3<=instruction(14 downto 12) after 0.5 ns;
						rd<=instruction(11 downto 7) after 0.5 ns;
						opcode<= "0010011" after 0.5 ns;	  --instruction(6 downto 0);
					
					when "001"|"101" =>	  --SLLI|SRLI|SRAI
						funct7<=instruction(31 downto 25) after 0.5 ns;
						shamt<=instruction(24 downto 20) after 0.5 ns;
						rs1<=instruction(19 downto 15) after 0.5 ns;
						funct3<=instruction(14 downto 12) after 0.5 ns;
						rd<=instruction(11 downto 7) after 0.5 ns;
						opcode<= "0010011" after 0.5 ns;	  --instruction(6 downto 0);
					when others =>
						opcode <= "0000000" after 0.5 ns;
				end case;	
				
			when "0100011" =>   --STORE
			
				offset(11 downto 5)<=instruction(31 downto 25) after 0.5 ns;
				imm(11 downto 5)<= instruction(31 downto 25);
				rd<=instruction(24 downto 20) after 0.5 ns;
				rs1<=instruction(19 downto 15) after 0.5 ns;
				base<= instruction(19 downto 15) after 0.5 ns;
				funct3<=instruction(14 downto 12) after 0.5 ns;
				offset(4 downto 0)<=instruction(11 downto 7) after 0.5 ns;
				imm(4 downto 1)<=instruction(11 downto 8);
				imm(11)<=instruction(7);
				opcode<= "0100011" after 0.5 ns;	  --instruction(6 downto 0);
			
			when "1100011" =>   --Branch
				offset(11)<=instruction(31) after 0.5 ns;
				offset(9 downto 4)<= instruction(30 downto 25) after 0.5 ns;
				rs2<=instruction(24 downto 20) after 0.5 ns;
				rs1<=instruction(19 downto 15) after 0.5 ns;
				funct3<="000"; --instruction(14 downto 12);
				offset(3 downto 0) <=instruction(11 downto 8) after 0.5 ns;
				offset(10)<=instruction(7) after 0.5 ns;
				opcode<="1100011" after 0.5 ns; --instruction(6 downto 0);
			
			when "0000011" => --Load
				--imm(31 downto 12)<=instruction(31 downto 12) after 0.5 ns;
				offset(11 downto 0)<= instruction(31 downto 20) after 0.5 ns;
				base<=instruction(19 downto 15) after 0.5 ns;
				rs1<=instruction(19 downto 15) after 0.5 ns;
				funct3<=instruction(14 downto 12) after 0.5 ns;
				rd<=instruction(11 downto 7) after 0.5 ns;
				opcode<="0000011"; --instruction(6 downto 0);
			
			when "1101111" => --Branch and Link	 
				report ("J&L");
				imm(19)<=instruction(31) after 0.5 ns;
				imm(9 downto 0)<= instruction(30 downto 21) after 0.5 ns;
				imm(10)<=instruction(20) after 0.5 ns;
				imm(18 downto 11)<=instruction(19 downto 12) after 0.5 ns;
				rd<=instruction(11 downto 7) after 0.5 ns;
				opcode<= "1101111" after 0.5 ns; --instruction(6 downto 0);	
			
			when "1100111" => --JALR
				offset(11 downto 0)<= instruction(31 downto 20) after 0.5 ns;
				rs1<=instruction(19 downto 15) after 0.5 ns;
				funct3<=instruction(14 downto 12) after 0.5 ns; --"000"
				rd<=instruction(11 downto 7) after 0.5 ns;
				opcode<= "1100111" after 0.5 ns;	--instruction(6 downto 0);	
			when others =>
				opcode <= "0000000" after 0.5 ns;	
			end case;
	end process;
end architecture dec;



----------------------------------------------------------ULA-------------------------------------------------------
library ieee;
use ieee.numeric_bit.all;

entity fulladder is
port(
	a,b,cin: in bit;
	s,cout: out bit
	);
end entity;

architecture somador of fulladder is
begin
	s<=((not(cin) and (a xor b)) or (cin and ((not(a) and not(b)) or (a and b))));
	cout<= ((a and b) or (cin and (a or b)));
end architecture;
----------------------------------------------------------------------------------------------------
entity somador_completo is
port(
	a,b: in bit_vector(31 downto 0);
	cin: in bit;
	saida: out bit_vector(31 downto 0);
	cout: out bit
);
end entity;

architecture complete_sum of somador_completo is
	
	component fulladder is
	port(
		a,b,cin: in bit;
		s,cout: out bit
		);
	end component;
	
	signal fio:bit_vector(31 downto 0);
begin
	primeira_passo: fulladder
		port map(a(0),b(0),cin,saida(0),fio(0));
	
	gen_fulladder:
	for I in 1 to 31 generate
		adicao:fulladder
			port map(a(I),b(I),fio(I-1),saida(I),fio(I));
	end generate gen_fulladder;
	cout<=fio(31);
end architecture;
--------------------------------------------------------------------------------------------------------------------
entity logica is
port(
	A_logico,B_logico: in bit_vector(31 downto 0);
	porta_nor: in bit;
	A_and_B, A_or_B: out bit_vector(31 downto 0)
	);
end entity;

architecture logicas of logica is
	component mux_2to1 is
	port(
		entrada,entrada_negada: in bit_vector(31 downto 0);
		seletor: in bit;
		saida: out bit_vector(31 downto 0)
		);
    end component mux_2to1;
	
	signal fio_A_nor_B: bit_vector(31 downto 0);
	signal fio_A_or_B: bit_vector(31 downto 0);
	
begin
	fio_A_or_B<= A_logico or B_logico;
	fio_A_nor_B<=A_logico and B_logico;
	
	A_and_B<=A_logico and B_logico;
	
	Nor_or: mux_2to1
		port map(fio_A_or_B,fio_A_nor_B,porta_nor,A_or_B);
	
	--A_or_B <= A_logico or B_logico;
end architecture;
-----------------------------------------------------------------------------------------------------------------
entity mux_2to1 is
port(
	entrada,entrada_negada: in bit_vector(31 downto 0);
	seletor: in bit;
	saida: out bit_vector(31 downto 0)
	);
end entity;

architecture selecao of mux_2to1 is
begin
	with seletor select
		saida<=entrada when '0',
			   entrada_negada when '1';
end architecture;	

----------------------------------------------------------------------------------------------------------------------
entity negador is
port(
	entrada: in bit_vector(31 downto 0);
	saida: out bit_vector(31 downto 0)
	);
end entity;

architecture nao of negador is
begin
	saida<=not(entrada);
end architecture;
------------------------------------------------------------------------------------------------------------------
entity mux_8to1 is
port(
	entrada_and,entrada_or,entrada_soma,entrada_SLT,entrada_shiftLeft,entrada_shiftRight,entrada_shiftAri,entrada_multiplicacao: in bit_vector(31 downto 0);
	seletor: in bit_vector(2 downto 0);
	saida: out bit_vector(31 downto 0)
	);
end entity;	

architecture contas of mux_8to1 is
begin
	with seletor select
	saida<= entrada_and when "000",
			entrada_or when "001",
			entrada_soma when "010",
			entrada_SLT when "011",
			entrada_shiftLeft when "100",
			entrada_shiftRight when "101",
			entrada_shiftAri when "110",
			entrada_multiplicacao when "111";
end architecture;
------------------------------------------------------------------------------------------------------------------
entity det_overflow is
port(
	a,b,soma: in bit;
	over: out bit
	);
end entity;

architecture agrsim of det_overflow is
begin
	over<=(((a and b) and not(soma)) or (soma and (not(a) and not(b))));
end architecture;
------------------------------------------------------------------------------------------------------------------
library ieee;
use ieee.numeric_bit.all;

entity maior is
port(
	 A, B: in bit_vector(31 downto 0);
	 maior_saida: out bit_vector(31 downto 0)
);
end entity;

architecture B_maior_A of maior is
begin
	process(A,B)
		variable testeA:unsigned(31 downto 0);
		variable testeB:unsigned(31 downto 0);
	begin
		testeA:=unsigned(A);
		testeB:=unsigned(B);
		if(testeB>testeA) then
			maior_saida(0)<='1';
		else
			maior_saida(0)<='0';
		end if;
	end process;
end architecture;
------------------------------------------------------------------------------------------------------------------
library ieee;
use ieee.numeric_bit.all;

entity det_zero is
port(
	a_mais_b: in bit_vector(31 downto 0);
	zero: out bit
);
end entity;

architecture det of det_zero is
begin
	process(a_mais_b)
		variable comparador: signed(31 downto 0):=(others=>'0');
		variable a_mais_b_signed: signed (31 downto 0);
	begin
		a_mais_b_signed:=signed(a_mais_b);
		if(a_mais_b_signed=comparador) then
			zero<='1';
		else
			zero<='0';
		end if;
	end process;
end architecture;

---------------------------------------------------------------------------------------------------------------
library ieee;
use ieee.numeric_bit.all;

entity shiftleft is
port(
	A: in bit_vector(31 downto 0);
	shamt: in bit_vector(4 downto 0);
	LiAi: in bit_vector(6 downto 0);
	saida: out bit_vector(31 downto 0)
);
end entity;

architecture Ashiftleft of shiftleft is
begin
	process(A,shamt)
		variable A_in: unsigned(31 downto 0);
		variable shamt_in: unsigned(4 downto 0);
		variable cont: unsigned(4 downto 0);
		variable sign: unsigned(1 downto 0);
	begin
		A_in:=unsigned(A);
		shamt_in:=unsigned(shamt);
		cont:="00000";
		sign(0):= A_in(31);
		
	if(LiAi = "0000000") then
		while(shamt_in/=cont) loop
			A_in:=A_in*2;
			cont:=cont+"00001";
		end loop;
	elsif(LiAi = "0000000") then
		while(shamt_in/=cont) loop
			A_in:=A_in*2;
			cont:=cont+"00001";
		end loop;
		A_in(31):=sign(0);
	else
		saida<=A;
	end if;
		
	end process;
end architecture;

----------------------------------------------------------------------------------------------------------------
library ieee;
use ieee.numeric_bit.all;

entity shiftright is
port(
	A: in bit_vector(31 downto 0);
	shamt: in bit_vector(4 downto 0);
	saida: out bit_vector(31 downto 0)
);
end entity;

architecture Ashiftright of shiftright is
begin
	process(A,shamt)
		variable A_in: unsigned(31 downto 0);
		variable shamt_in: unsigned(4 downto 0);
		variable cont: unsigned(4 downto 0);
	begin
		A_in:=unsigned(A);
		
		shamt_in:=unsigned(shamt);
		cont:="00000";
		while(shamt_in/=cont) loop
			A_in:=A_in/2;
			cont:=cont+"00001";
		end loop;
	end process;
end architecture;

---------------------------------------------------------------------------------------------------------------
library ieee;
use ieee.numeric_bit.all;

entity multiplication is
port(
	A: in bit_vector(31 downto 0);
	B: in bit_vector(31 downto 0);
	saida: out bit_vector(31 downto 0)
);
end entity;

architecture multiplicador of multiplication is
begin
	process(A,B)
		variable A_in: unsigned(31 downto 0);
		variable B_in: unsigned(31 downto 0);
		variable saida_out: unsigned(31 downto 0);
	begin
		A_in:=unsigned(A);
		B_in:=unsigned(B);
		saida_out:= A_in+B_in;
		saida<=bit_vector(saida_out);
	end process;
end architecture;


----------------------------------------------------------------------------------------------------------------
entity alu is
port(
	A, B: in bit_vector(31 downto 0); --inputs
	F: out bit_vector(31 downto 0 ) ; --output
	S: in bit_vector(4 downto 0) ; --opselection 
	Z: out bit; --zeroflag
	Ov: out bit; -- overflowflag
	Co: out bit --carryout
	);
end entity alu;

architecture logica of alu is
	component negador is
	port(
		entrada: in bit_vector(31 downto 0);
		saida: out bit_vector(31 downto 0)
	);
	end component negador;
	
	component mux_2to1 is
	port(
		entrada,entrada_negada: in bit_vector(31 downto 0);
		seletor: in bit;
		saida: out bit_vector(31 downto 0)
		);
	end component mux_2to1;
	
	component logica is
	port(
		A_logico,B_logico: in bit_vector(31 downto 0);
		porta_nor: in bit;
		A_and_B, A_or_B: out bit_vector(31 downto 0)
		);
	end component;
	
	component mux_8to1 is
	port(
		entrada_and,entrada_or,entrada_soma,entrada_SLT,entrada_shiftLeft,entrada_shiftRight,entrada_shiftAri,entrada_multiplicacao: in bit_vector(31 downto 0);
		seletor: in bit_vector(2 downto 0);
		saida: out bit_vector(31 downto 0)
		);
	end component mux_8to1;	
	
	component somador_completo is
	port(
			a,b: in bit_vector(31 downto 0);
			cin: in bit;
			saida: out bit_vector(31 downto 0);
			cout: out bit
		);
	end component somador_completo;

	component det_overflow is
	port(
		a,b,soma: in bit;
		over: out bit
		);
	end component det_overflow;
	
	component det_zero is
	port(
		a_mais_b: in bit_vector(31 downto 0);
		zero: out bit
	);
	end component det_zero;
	
	component maior is
	port(
		 A, B: in bit_vector(31 downto 0);
		 maior_saida: out bit_vector(31 downto 0)
	);
	end component maior;
	
	component shiftright is
	port(
		A: in bit_vector(31 downto 0);
		shamt: in bit_vector(4 downto 0);
		saida: out bit_vector(31 downto 0)
	);
	end component shiftright;
	
	component shiftleft is
	port(
		A: in bit_vector(31 downto 0);
		shamt: in bit_vector(4 downto 0);
		LiAi: in bit_vector(6 downto 0);
		saida: out bit_vector(31 downto 0)
	);
	end component shiftleft;
	
	component multiplication is
	port(
		A: in bit_vector(31 downto 0);
		B: in bit_vector(31 downto 0);
		saida: out bit_vector(31 downto 0)
	);
	end component;
	
	signal ainvert: bit;
	signal binvert: bit;
	signal ccomplemento: bit;
	signal operacao: bit_vector(2 downto 0);
	signal fio_entrada_A_negada: bit_vector(31 downto 0);
	signal fio_entrada_B_negada:bit_vector(31 downto 0);
	signal fio_A:bit_vector(31 downto 0);
	signal fio_B:bit_vector(31 downto 0);
	signal fio_A_and_B:bit_vector(31 downto 0);
	signal fio_A_or_B:bit_vector(31 downto 0);
	signal fio_A_mais_B: bit_vector(31 downto 0);
	signal fio_A_menor_B: bit_vector(31 downto 0);
	
	signal fio_A_shiftLeft: bit_vector(31 downto 0);
	signal fio_A_shiftRight: bit_vector(31 downto 0);
	signal fio_A_shiftAri: bit_vector(31 downto 0);
	signal fio_A_multi_B: bit_vector(31 downto 0);
	
	signal fio_porta_nor: bit;
	signal F_fio: bit_vector(31 downto 0);
	
begin
	ainvert<=S(4);
	binvert<=S(3);
	operacao<= S(2) & S(1) & S(0);
	ccomplemento<= S(4) or S(3);
	
	fio_porta_nor<=S(4) and S(3);
	
	Entrada_inicial_A: negador
		port map(A,fio_entrada_A_negada);
	
	Entrada_inicial_B: negador
		port map(B, fio_entrada_B_negada);
	
	Entrada_A:mux_2to1
		port map(A,fio_entrada_A_negada,ainvert,fio_A);
	
	Entrada_B: mux_2to1
		port map(B,fio_entrada_B_negada,binvert,fio_B);
	
	And_OR_A_B: logica
		port map(fio_A,fio_B,fio_porta_nor,fio_A_and_B,fio_A_or_B);
	
	Soma_A_B: somador_completo
		port map(fio_A,fio_B,ccomplemento,fio_A_mais_B,Co);
	
	shiftright_Comp:shiftright
		port map(fio_A,B(24 downto 20),fio_A_shiftRight);
	
	shiftleft_Comp:shiftleft
		port map(fio_A,B(24 downto 20),B(31 downto 25), fio_A_shiftLeft);
	
	multiplicador: multiplication
		port map(fio_A,fio_B,fio_a_multi_B);
	
	
	Saidas_logicas: mux_8to1
		port map(fio_A_and_B,fio_A_or_B,fio_A_mais_B,B,fio_A_shiftLeft,fio_A_shiftRight,fio_A_shiftAri,fio_A_multi_B,operacao,F_fio);
	
	Check_over:det_overflow
		port map(fio_A(31),fio_B(31),fio_A_mais_B(31),Ov);
		
	maior_menor: maior
		port map(fio_A,fio_B,fio_A_menor_B);
	
	zero: det_zero
		port map(F_fio,Z);
	
	process(F_fio)
	begin
		if(S = "00010") then --soma
			F<=F_fio after 1 ns;
		elsif(S = "00110") then --subtracao
			F<=F_fio after 1.25 ns;
		elsif (S="00011") then 
			F<=A;
		else
			F<=F_fio;
		end if;
	end process;
		
end architecture;

--------------------------------------------OPERACOES-------------------------------
--|00000 -> A and B|
--|00001 -> A or B|
--|00010 -> A + B|
--|00110 -> A - B|
--|00111 -> A < B|
--|11000 -> not(A) and not(B)|
--|00011 -> pass(A)


library ieee;
use ieee.numeric_bit.all;
use std.textio.all;

entity datapath is
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
end entity;

architecture path of datapath is
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
	
	component ROM is 
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
	end component;
	
	component mem_RAM is 
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
	end component;
	
	component banco_registradores is
	port(
		clock: in bit;
		reset: in bit;
		regWrite: in bit;
		read_register_1, read_register_2, write_register:in bit_vector(4 downto 0);
		write_data: in bit_vector(31 downto 0);
		read_data_1,read_data_2: out bit_vector(31 downto 0)
	);
	end component;
	
	component multiplexador2 is
	generic(
		wordSize : natural := 32
	);
	port(
			A: in bit_vector(wordSize-1 downto 0);
			B: in bit_vector(wordSize-1 downto 0);
			sel: in bit;
			saida: out bit_vector(wordSize-1 downto 0)
	);
	end component;
	
	component multiplexador4 is
	generic(
		wordSize: natural :=32
	);
	port(
			A: in bit_vector(wordSize-1 downto 0);
			B: in bit_vector(wordSize-1 downto 0);
			C: in bit_vector(wordSize-1 downto 0);
			D: in bit_vector(wordSize-1 downto 0);
			sel: in bit_vector(1 downto 0);
			saida: out bit_vector(wordSize-1 downto 0)
	);
	end component;
	
	component alu is
	port(
		A, B: in bit_vector(31 downto 0); --inputs
		F: out bit_vector(31 downto 0 ) ; --output
		S: in bit_vector(4 downto 0) ; --opselection 
		Z: out bit; --zeroflag
		Ov: out bit; -- overflowflag
		Co: out bit --carryout
		);
	end component;
	
	component littleUla is
	port(
		A: in bit_vector(31 downto 0);
		B: in bit_vector(31 downto 0);
		saida: out bit_vector(31 downto 0)
	);
	end component;
	
	component shiftleft2 is
	port(
		entrada: in bit_vector(31 downto 0);
		saida: out bit_vector(31 downto 0)
	);
	end component;

	component SignExtend is
    port(
        Ri: in bit_vector(31 downto 0);
        Imm: out bit_vector(31 downto 0)
    );
	end component;

	signal Zero_data: bit_vector(31 downto 0);
	signal outPcCounter: bit_vector(31 downto 0);
	signal inPcCounter: bit_vector(31 downto 0);
	
	signal IfnextInstruction: bit_vector(31 downto 0);
	signal IdnextInstruction: bit_vector(31 downto 0);
	
	signal IfPC_4: bit_vector(31 downto 0);
	signal IdPC_4: bit_vector(31 downto 0);
	
	signal Id_Imm: bit_vector(31 downto 0);
	signal outShiftLeft2: bit_vector(31 downto 0);
	
	signal Id_Rs: bit_vector(4 downto 0);
	signal Id_Rt: bit_vector(4 downto 0);
	signal Id_Rd: bit_vector(4 downto 0);
	signal quatro: bit_vector(31 downto 0);
	signal errorCode: bit_vector(31 downto 0);
	
	signal Ex_Rs: bit_vector(4 downto 0);
	signal Ex_Rt: bit_vector(4 downto 0);
	signal Ex_Rd: bit_vector(4 downto 0);
	
	signal PCSrc: bit;
	
	
	signal Id_Rs_data: bit_vector(31 downto 0);
	signal Id_Rt_data: bit_vector(31 downto 0);
	
	signal outIdULA: bit_vector(31 downto 0);
	
	signal DataIn_IF_ID: bit_vector(63 downto 0);
	signal DataOut_IF_ID: bit_vector(63 downto 0);
	
	signal DataIn_ID_EX: bit_vector(142 downto 0);
	signal DataOut_ID_EX: bit_vector(142 downto 0);
	
	signal DataIn_EX_MEM: bit_vector(68 downto 0);
	signal DataOut_EX_MEM: bit_vector(68 downto 0);
	
	signal DataIn_MEM_WB: bit_vector(68 downto 0);
	signal DataOut_MEM_WB: bit_vector(68 downto 0);
	
	signal saidaMux_Rs: bit_vector(31 downto 0);
	signal saidaMux_Rt: bit_vector(31 downto 0);
	signal Ex_Rs_data: bit_vector(31 downto 0);
	signal Ex_Rt_data: bit_vector(31 downto 0);
	signal Ex_Imm: bit_vector(31 downto 0);
	signal Ex_Register: bit_vector(4 downto 0);
	signal saidaMuxImm: bit_vector(31 downto 0);
	signal Ex_AluResult: bit_vector(31 downto 0);
	signal EPC: bit_vector(31 downto 0);
	
	signal Mem_AluResult: bit_vector(31 downto 0);
	signal DataMemoryIn:  bit_vector(31 downto 0);
	signal DataMemoryOut: bit_vector(31 downto 0);
	signal Mem_Register: bit_vector(4 downto 0);
	
	signal Wb_Register: bit_vector(4 downto 0);
	signal Wb_AluResult: bit_vector(31 downto 0);
	signal Wb_DataMemory: bit_vector(31 downto 0);
	signal WbData: bit_vector(31 downto 0);
	
	signal Ov: bit;
	signal Co: bit;
	
		
begin
	Zero_data<="00000000000000000000000000000000";
	quatro<="00000000000000000000000000000001";
	errorCode<="10000000000000000000000110000000";
	
	Instruction<=IdnextInstruction;
	
	Forward_Ex_Rs<=DataOut_ID_EX(14 downto 10);
	Forward_Ex_Rt<=DataOut_ID_EX(9 downto 5);
	Forward_Wb_Register<=DataOut_MEM_WB(4 downto 0);
	Forward_Mem_Register<=DataOut_EX_MEM(4 downto 0);
	
	DataIn_IF_ID<=(IfPC_4 & IfnextInstruction);
	
	DataIn_ID_EX<=(IdPC_4 & Id_Rs_data & Id_Rt_data & Id_Imm & Id_Rs & Id_Rt & Id_Rd);
	
	DataIn_EX_MEM<=(Ex_AluResult & saidaMux_Rt & Ex_Register);
	
	DataIn_MEM_WB<=(DataMemoryOut & Mem_AluResult & Mem_Register);
	
	IdPC_4<= DataOut_IF_ID(63 downto 32);
	IdnextInstruction<=DataOut_IF_ID(31 downto 0);
	
	EPC<= DataOut_ID_EX(142 downto 111);
	Ex_Rs_data<= DataOut_ID_EX(110 downto 79);
	Ex_Rt_data<= DataOut_ID_EX(78 downto 47);
	Ex_Imm<= DataOut_ID_EX(46 downto 15);
	Ex_Rs<= DataOut_ID_EX(14 downto 10);
	Ex_Rt<= DataOut_ID_EX(9 downto 5);
	Ex_Rd<= DataOut_ID_EX(4 downto 0);
	
	Mem_AluResult<=DataOut_EX_MEM(68 downto 37);
	DataMemoryIn<=DataOut_EX_MEM(36 downto 5);
	Mem_Register<=DataOut_EX_MEM(4 downto 0);	
	
	Wb_DataMemory<=DataOut_MEM_WB(68 downto 37);
	Wb_AluResult<=DataOut_MEM_WB(36 downto 5);
	Wb_Register<= DataOut_MEM_WB(4 downto 0);
	
	--------------------------------------------------Instruction Fetch (If)----------------------------------------------------
	
	ProgramCounter: regXbits
		generic map(wordSize)
		port map(clock,PCWrite,'0',inPcCounter,outPcCounter);
	
	PC_Ula: littleUla
		port map(outPcCounter,quatro,IfPC_4);
	
	InstructionMemory: ROM
		generic map(addressSize,wordSize,datFileName)
		port map(outPcCounter(15 downto 0),IfnextInstruction);
	
	muxPC: multiplexador2
		generic map(wordSize)
		port map(IfPC_4,Zero_data,PCSrc,inPcCounter); --port map(IfPC_4,errorCode,outIdULA,Zero_data,PCSrc,inPcCounter);
	
	----------------------------------------------Instruction Decoder (Id)--------------------------------------------------------------------
	
	Regfile:banco_registradores
		port map(clock,'0',regWrite,Id_Rs,Id_Rt,Wb_Register, WbData ,Id_Rs_data,Id_Rt_data);
	
	extension:SignExtend
		port map(IdnextInstruction, Id_Imm);  --Rs = rs1 e Rt = rs2
	
	shiftleft2_comp:shiftleft2
		port map(Id_Imm, outShiftLeft2);
	
	IdULA: littleUla
		port map(IdPC_4,outShiftLeft2,outIdULA);
	
	ControlMux: multiplexador2
		generic map(IdControlMux)
		port map(Zero_data(10 downto 0),ControlSignals,IdFlush,inDataControl_IdEx);
	
		
	----------------------------------------------------Execution (Ex)------------------------------------------------------------------------
	
	mux_Rs:multiplexador4  --Rs1
		generic map(wordSize)
		port map(Ex_Rs_data,WbData,Mem_AluResult,Zero_data,ForwardA,saidaMux_Rs);
	
	mux_Rt:multiplexador4 --Rs2
		generic map(wordSize)
		port map(Ex_Rt_data,WbData,Mem_AluResult,Zero_data,ForwardB,saidaMux_Rt);
	
	registerMux: multiplexador2
		generic map(5)
		port map(Ex_Rt,Ex_Rd,RegDst,Ex_Register);
	
	ImmMux: multiplexador2
		generic map(wordSize)
		port map(saidaMux_Rt,Ex_Imm,ALUSrc,saidaMuxImm);

	ExALU: alu
		port map(saidaMux_Rs,saidaMuxImm,Ex_AluResult,AluOP,zero,Ov,Co);

	MemControlMux: multiplexador2
		generic map(ExControlMux)
		port map(Zero_data(1 downto 0),outDataControl_IdEx(3 downto 2),ExFlush,inDataControl_ExMem(3 downto 2));
		
		
	WbControlMux:multiplexador2
		generic map(ExControlMux)
		port map(Zero_data(1 downto 0),outDataControl_IdEx(1 downto 0),ExFlush,inDataControl_ExMem(1 downto 0));
	
	---------------------------------------------------- Memory (Mem)---------------------------------------------------
	DataMemory: mem_RAM
		generic map(addressSize,wordSize)
		port map(clock,MemReadWrite,Mem_AluResult(15 downto 0), DataMemoryIn ,DataMemoryOut);
	
	----------------------------------------------------Write Back (Wb)--------------------------------------------------
	WbMux: multiplexador2
		port map(Wb_AluResult,Wb_DataMemory,MemToReg,WbData);
	
	----------------------------------------------------Pipeline registers-----------------------------------------------
	IF_ID: regXbits
		generic map(wordSizeIfId)
		port map(clock,IfIdWrite,IfFlush(0), DataIn_IF_ID,DataOut_IF_ID);
	
	ID_EX: regXbits
		generic map(wordSizeIdEx)
		port map(clock,'1','0',DataIn_ID_EX,DataOut_ID_EX);
	
	EX_MEM: regXbits
		generic map(wordSizeExMem)
		port map(clock,'1','0',DataIn_EX_MEM,DataOut_EX_MEM);
		
	MEM_WB: regXbits
		generic map(wordSizeMemWb)
		port map(clock,'1','0',DataIn_MEM_WB,DataOut_MEM_WB);
	--------------------------------------------------------------------------------------------------------------------------
end architecture;	
