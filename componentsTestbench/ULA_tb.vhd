entity ULAtb is
end ULAtb ;

architecture arch of ULAtb is

    component alu is
        port(
            A, B: in bit_vector(31 downto 0); --inputs
            F: out bit_vector(31 downto 0 ) ; --output
            S: in bit_vector(3 downto 0 ) ; --opselection 
            Z: out bit; --zeroflag
            Ov: out bit; -- overflowflag
            Co: out bit --carryout
            );
        end component;


    signal A,B,F: bit_vector(31 downto 0);
    signal S: bit_vector(3 downto 0);
    signal Z,Ov,Co: bit;

begin

    DUT: alu port map(A,B,F,S,Z,Ov,Co);

    stimulus: process is
        begin
        assert false report "simulation start" severity note;
        
        A<="11111111111111111111111111111111";
        B<="10101010101010101010101010101010";
        S<="0000";
        wait for 1 ns;
        A<="00000000000000000000000000000000";
        B<="10101010101010101010101010101010";
        S<="0001";
        wait for 1 ns;
        A<="00000000000000000000000000000001";
        B<="00000000000000000000000000000001";
        S<="0010";
        wait for 1 ns;
        A<="00000000000000000000000000000001";
        B<="00000000000000000000000000000001";
        S<="0110";
        wait for 1 ns;
        A<="00000000000010000000000000000000";
        B<="00000000000000000000001000000001";
        S<="0111";
        wait for 1 ns;
        A<="00000000000010000000000000000000";
        B<="00000000000000000000001000000001";
        S<="1100";
        wait for 1 ns;
        A<="00000000000010000000000000000000";
        B<="00010000000000000000001000000001";
        S<="0011";
        wait for 1 ns;
        
        assert false report "Tudo certo" severity note;
        wait;
        end process;