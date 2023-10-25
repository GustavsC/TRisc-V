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