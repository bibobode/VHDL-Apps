library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--
-- 7-segment display driver. It displays a 4-bit number on 7-segments 
-- This is created as an entity so that it can be reused many times easily
--

entity SevenSegment is port (
   
   dataIn      :  in  std_logic_vector(3 downto 0);   -- The 4 bit data to be displayed
   blanking    :  in  std_logic;                      -- This bit turns off all segments
   
   segmentsOut :  out std_logic_vector(6 downto 0)    -- 7-bit outputs to a 7-segment
); 
end SevenSegment;

architecture Behavioral of SevenSegment is

-- 
-- The following statements convert a 4-bit input, called dataIn to a pattern of 7 bits
-- The segment turns on when it is '0' otherwise '1'
-- The blanking input is added to turns off the all segments
--

begin

   with blanking & dataIn select --  gfedcba        b3210      -- D7S
      segmentsOut(6 downto 0) <=    "1000000" when "00000",    -- [0]
                                    "1111001" when "00001",    -- [1]
                                    "0100100" when "00010",    -- [2]      +---- a ----+
                                    "0110000" when "00011",    -- [3]      |           |
                                    "0011001" when "00100",    -- [4]      |           |
                                    "0010010" when "00101",    -- [5]      f           b
                                    "0000010" when "00110",    -- [6]      |           |
                                    "1111000" when "00111",    -- [7]      |           |
                                    "0000000" when "01000",    -- [8]      +---- g ----+
                                    "0010000" when "01001",    -- [9]      |           |
                                    "0001000" when "01010",    -- [A]      |           |
                                    "0000011" when "01011",    -- [b]      e           c
                                    "0100111" when "01100",    -- [c]      |           |
                                    "0100001" when "01101",    -- [d]      |           |
                                    "0000110" when "01110",    -- [E]      +---- d ----+
                                    "0001110" when "01111",    -- [F]
                                    "1111111" when others;     -- [ ]

end Behavioral;

--------------------------------------------------------------------------------
-- Main entity
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Lab2 is port (
      
      key              :   in  std_logic_vector( 3 downto 0); -- 4 push buttons
      sw               :   in  std_logic_vector(17 downto 0); -- 18 dip switches

      ledr             :   out std_logic_vector(17 downto 0); -- 18 red LEDs
      hex0, hex1, hex2, hex4, hex5, hex6, hex7			   :   out std_logic_vector( 6 downto 0)  -- 7-segment desplays
);
end Lab2;

architecture SimpleCircuit of Lab2 is

--
-- In order to use the "SevenSegment" entity, we should declare it with first
-- 

   component SevenSegment port (
      dataIn      :  in    std_logic_vector(3 downto 0);
      blanking    :  in    std_logic;
      segmentsOut :  out   std_logic_vector(6 downto 0)
   );
   end component;

-- Create any signals, or temporary variables to be used
--
-- Note that there are two basic types and mixing them is difficult
--  unsigned is a signal which can be used to perform math operations such as +, -, *
--  std_logic_vector is a signal which can be used for logic operations such as OR, AND, NOT, XOR
--
   signal A, B, C, D: std_logic_vector(3 downto 0); -- 4-bit intermediate signals (wires)
   signal S: std_logic_vector(1 downto 0); -- 2-bit intermediate signals (wires)
   signal R1, R2, R3: std_logic_vector(3 downto 0); -- 4-bit intermediate signals (wires)
   
   signal C1, C2, C3: std_logic_vector(11 downto 0); -- 12-bit intermediate signals (wires)
   

-- Here the circuit begins

begin

-- intermediate signal assignments

   A <= sw( 3 downto 0);  -- connect the lowest 3 switches to A
   B <= sw (7 downto 4);  -- connect the next 3 switches to B	 
   C <= sw (11 downto 8); -- connect the next 3 switches to C
   D <= sw (15 downto 12); -- connect the next 3 switches to D
   S <= sw (17 downto 16); -- connect switches 17 to 16 to S -> S determines the operator

   -- combine values of each digit into a container signal for operation
   C1 <= ("0000" & B & A);
   C2 <= ("0000" & D & C);
   
   with sw(17 downto 16) select
		C3 <= C1 and C2 when "00",
			  C1 or C2 when "01",
			  C1 xor C2 when "10",
			  std_logic_vector (unsigned (C1) + unsigned (C2)) when others;
			  
   
   R1 <= C3 (11 downto 8);
   R2 <= C3 (7 downto 4);
   R3 <= C3 (3 downto 0);		
   
-- signal is assigned to LED

   ledr( 3 downto  0) <= R3;
   ledr (7 downto 4) <= R2;
   ledr (11 downto 8) <= R1;
   ledr (17 downto 16) <= S;

-- signal is sidplayed on seven-segment. '0' is concatenated with signal to make a 5-bit input

   D7SH4: SevenSegment port map(A, '0', hex4 ); -- A is diplayed on HEX4, blanking is disabled
   D7SH5: SevenSegment port map(B, '0', hex5 ); -- B is diplayed on HEX5, blanking is disabled
   D7SH6: SevenSegment port map(C, '0', hex6 ); -- C is diplayed on HEX6, blanking is disabled
   D7SH7: SevenSegment port map(D, '0', hex7 ); -- D is diplayed on HEX7, blanking is disabled
   D7SH0: SevenSegment port map(R3, '0', hex0 ); -- R3 is diplayed on HEX0, blanking is disabled
   D7SH1: SevenSegment port map(R2, '0', hex1 ); -- R2 is diplayed on HEX1, blanking is disabled
   D7SH2: SevenSegment port map(R1, not R1(0), hex2 ); -- R1 is diplayed on HEX2, blanking is dependant on value of R1(0)
   
end SimpleCircuit;
