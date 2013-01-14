LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

--
-- 7-segment display driver. It displays a 4-bit number on 7-segments 
-- This is created as an entity so that it can be reused many times easily
--

ENTITY SevenSegment IS PORT (
   
   dataIn      :  IN  std_logic_vector(3 DOWNTO 0);   -- The 4 bit data to be displayed
   blanking    :  IN  std_logic;                      -- This bit turns off all segments
   
   segmentsOut :  OUT std_logic_vector(6 DOWNTO 0)    -- 7-bit outputs to a 7-segment
); 
END SevenSegment;

ARCHITECTURE Behavioral OF SevenSegment IS

-- 
-- The following statements convert a 4-bit input, called dataIn to a pattern of 7 bits
-- The segment turns on when it is '0' otherwise '1'
-- The blanking input is added to turns off the all segments
--

BEGIN

   with blanking & dataIn SELECT --  gfedcba        b3210      -- D7S
      segmentsOut(6 DOWNTO 0) <=    "1000000" WHEN "00000",    -- [0]
                                    "1111001" WHEN "00001",    -- [1]
                                    "0100100" WHEN "00010",    -- [2]      +---- a ----+
                                    "0110000" WHEN "00011",    -- [3]      |           |
                                    "0011001" WHEN "00100",    -- [4]      |           |
                                    "0010010" WHEN "00101",    -- [5]      f           b
                                    "0000010" WHEN "00110",    -- [6]      |           |
                                    "1111000" WHEN "00111",    -- [7]      |           |
                                    "0000000" WHEN "01000",    -- [8]      +---- g ----+
                                    "0010000" WHEN "01001",    -- [9]      |           |
                                    "0001000" WHEN "01010",    -- [A]      |           |
                                    "0000011" WHEN "01011",    -- [b]      e           c
                                    "0100111" WHEN "01100",    -- [c]      |           |
                                    "0100001" WHEN "01101",    -- [d]      |           |
                                    "0000110" WHEN "01110",    -- [E]      +---- d ----+
                                    "0001110" WHEN "01111",    -- [F]
                                    "1111111" WHEN OTHERS;     -- [ ]

END Behavioral;

--------------------------------------------------------------------------------
-- Main entity
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY Lab4 IS
   PORT(
      
      clock_50   : IN  STD_LOGIC;
      sw         : IN  STD_LOGIC_VECTOR(17 DOWNTO 0); -- 18 dip switches on the board

      ledr       : OUT STD_LOGIC_VECTOR(17 DOWNTO 0); -- LEDs, many Red ones are available
      ledg       : OUT STD_LOGIC_VECTOR( 8 DOWNTO 0); -- LEDs, many Green ones are available
      hex0, hex2, hex4, hex6 : OUT STD_LOGIC_VECTOR( 6 DOWNTO 0)  -- seven segments to display numbers
);
END Lab4;

ARCHITECTURE SimpleCircuit OF Lab4 IS

--
-- In order to use the "SevenSegment" entity, we should declare it with first
-- 

   COMPONENT SevenSegment PORT(        -- Declare the 7 segment component to be used
      dataIn      : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      blanking    : IN  STD_LOGIC;
      segmentsOut : OUT STD_LOGIC_VECTOR(6 DOWNTO 0)
   );
   END COMPONENT;
----------------------------------------------------------------------------------------------------
   CONSTANT CLK_DIV_SIZE: INTEGER := 25;     -- size of vectors for the counters

   SIGNAL MainCLK:   STD_LOGIC; -- main clock to drive FSM
   SIGNAL OneHzBinCLK:  STD_LOGIC; -- binary 1 Hz clock
   SIGNAL OneHzModCLK:  STD_LOGIC; -- modulus1 Hz clock
   SIGNAL TenHzModCLK:  STD_LOGIC; -- modulus1 Hz clock
   SIGNAL MsetModCLK:  STD_LOGIC; -- set by SW[2-0]

   SIGNAL bin_counter:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset binary counter to zero
   SIGNAL mod_counter:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset modulus counter to zero
   SIGNAL mod1_counter:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset modulus counter to zero
   SIGNAL mod10_counter:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset modulus counter to zero
   SIGNAL mod_terminal: UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset terminal count of modulus counter to zero
   
   TYPE STATES IS (STATE0, STATE1, STATE2, STATE3);   -- list all the STATES
   SIGNAL state, next_state:  STATES;                 -- current and next state signals od type STATES
   
   TYPE MODE IS (DAY,NIGHT);
   SIGNAL day_state: MODE;
 
   TYPE DIR IS (NS,EW);
   SIGNAL def_side: DIR;
   
   SIGNAL state_number: STD_LOGIC_VECTOR(3 DOWNTO 0); -- binary state number to display on seven-segment

   SIGNAL state_counter: UNSIGNED(3 DOWNTO 0);        -- binary state counter to display on seven-segment
   SIGNAL state0counter: UNSIGNED(3 DOWNTO 0);        -- binary state counter to display on seven-segment
   SIGNAL state1counter: UNSIGNED(3 DOWNTO 0);        -- binary state counter to display on seven-segment
   SIGNAL state2counter: UNSIGNED(3 DOWNTO 0);        -- binary state counter to display on seven-segment
   SIGNAL state3counter: UNSIGNED(3 DOWNTO 0);        -- binary state counter to display on seven-segment
   SIGNAL wait_counter_ns: UNSIGNED(3 DOWNTO 0);        -- binary state counter to display on seven-segment
   SIGNAL wait_counter_ew: UNSIGNED(3 DOWNTO 0);        -- binary state counter to display on seven-segment
----------------------------------------------------------------------------------------------------
-- BINARY CLOCK
BEGIN

   BinCLK: PROCESS(clock_50)
   BEGIN
      IF (rising_edge(clock_50)) THEN -- binary counter increments on rising clock edge
         bin_counter <= bin_counter + 1;
      END IF;
   END PROCESS;
   OneHzBinCLK <= std_logic(bin_counter(CLK_DIV_SIZE-1)); -- binary counter MSB
   LEDG(2) <= OneHzBinCLK;
   
----------------------------------------------------------------------------------------------------
-- 10 Hz MOD Clock

   --Mod10CLK: PROCESS(clock_50)
   --BEGIN
   --   IF (rising_edge(clock_50)) THEN -- modulus counter increments on rising clock edge
   --      IF (mod10_counter = "0001001100010010110100000") THEN       -- half period
   --         TenHzModCLK <= NOT TenHzModCLK;                 -- toggle
   --        mod10_counter <= to_unsigned(0,CLK_DIV_SIZE); -- reset counter
            
   --      ELSE
   --         mod10_counter <= mod10_counter + 1;
   --      END IF;
   --   END IF;
		
   --END PROCESS;
   TenHzModCLK <= clock_50;
   LEDG(0) <= TenHzModCLK;

----------------------------------------------------------------------------------------------------
-- 1 Hz MOD Clock (from 10 Hz MOD Clock)

   Mod1CLK: PROCESS(TenHzModCLK)
   BEGIN
      IF (rising_edge(TenHzModCLK)) THEN -- modulus counter increments on rising clock edge
         IF (mod1_counter = "0000000000000000000000100") THEN
            -- NOTE: WE MADE IT GO TO 4 BECAUSE WE RECEIVE THE RISING EDGE A CYCLE LATER FROM THE 10 HZ CLOCK
            OneHzModCLK <= NOT OneHzModCLK;                 -- toggle
            mod1_counter <= to_unsigned(0,CLK_DIV_SIZE); -- reset counter
         ELSE
            mod1_counter <= mod1_counter + 1;
         END IF;
      END IF;
   END PROCESS;
   LEDG(1) <= OneHzModCLK;
   
----------------------------------------------------------------------------------------------------
-- MANUAL SETTING MOD CLOCK

   WITH sw(2 DOWNTO 0) SELECT -- terminal count for modulus counter
      mod_terminal <= "1011111010111100001000000" WHEN "000",  -- F = 1 Hz, T = 1 sec
                      "0011111110010100000010101" WHEN "001",  -- F = 3 Hz, T = 0.333 sec
                      "0001001100010010110100000" WHEN "010",  -- F = 10 Hz, T = 0.1 sec
                      "0000000111101000010010000" WHEN "011",  -- F = 100 Hz, T = 0.01 sec
                      "1011111010111100001000000" WHEN OTHERS; -- F = 1 Hz, T = 1 sec; *** default ***

   ModCLK: PROCESS(clock_50) 
   BEGIN
      IF (rising_edge(clock_50)) THEN -- modulus counter increments on rising clock edge
         IF (mod_counter = mod_terminal) THEN       -- half period
            MsetModCLK <= NOT MsetModCLK;                 -- toggle
            mod_counter <= to_unsigned(0,CLK_DIV_SIZE); -- reset counter
         ELSE
            mod_counter <= mod_counter + 1;
         END IF;
      END IF;
   END PROCESS;
   
----------------------------------------------------------------------------------------------------
   MainCLK <= TenHzModCLK;
----------------------------------------------------------------------------------------------------
   FSM: PROCESS(state, sw) -- main FSM
   BEGIN
      next_state <= state;
      --ledr(15 DOWNTO 0) <= "0000000000000000";
      
      IF (sw(17) = '0') THEN
		 day_state <= DAY;
	  ELSE
		 day_state <= NIGHT;
      END IF;
      
      IF (sw(16) = '0') THEN
		 def_side <= NS;
	  ELSE
		 def_side <= EW;
      END IF;
      
	
   CASE state IS
	
	  WHEN STATE0=>
		 state_number <= "0000";
		 IF (state0counter < "0010") THEN
			ledg(8) <= TenHzModCLK;
			ledr(11) <= '0';
			ledg(7) <= '0';
			ledr(0) <= '1';
			next_state <= STATE0;
         ELSIF (state0counter > "0001" and state0counter < "0110") THEN
			ledg(8) <= '1';
			ledr(11) <= '0';
			ledg(7) <= '0';
			ledr(0) <= '1';
			next_state <= STATE0;
		 ELSIF (state0counter > "0101" and state0counter < "0111") THEN
			ledg(8) <= '0';
			ledr(11) <= TenHzModCLK;
			ledg(7) <= '0';
			ledr(0) <= '1';
			next_state <= STATE0;
		 ELSE
			ledg(8) <= '0';
			ledr(11) <= TenHzModCLK;
			ledg(7) <= '0';
			ledr(0) <= '1';		
			IF (day_state = NIGHT and def_side = NS and sw(14) = '0') THEN
				next_state <= STATE2;
			ELSE
				next_state <= STATE1;
			END IF;
		 END IF;
		 
	  WHEN STATE1=>
		 state_number <= "0001";
		 IF (state1counter < "0010") THEN
			ledg(8) <= '0';
			ledr(11) <= '1';
			ledg(7) <= TenHzModCLK;
			ledr(0) <= '0';
			next_state <= STATE1;
         ELSIF (state1counter > "0001" and state1counter < "0110") THEN
			ledg(8) <= '0';
			ledr(11) <= '1';
			ledg(7) <= '1';
			ledr(0) <= '0';
			next_state <= STATE1;
		 ELSIF (state1counter > "0101" and state1counter < "0111") THEN
			ledg(8) <= '0';
			ledr(11) <= '1';
			ledg(7) <= '0';
			ledr(0) <= TenHzModCLK;
			next_state <= STATE1;
		 ELSE
			ledg(8) <= '0';
			ledr(11) <= '1';
			ledg(7) <= '0';
			ledr(0) <= TenHzModCLK;	
			IF (day_state = NIGHT and def_side = EW and sw(15) = '0') THEN
				next_state <= STATE3;
			ELSE
				next_state <= STATE0;
			END IF;
		 END IF;		 
		 
	  WHEN STATE2=>
	     state_number <= "0010";
         IF (state2counter < "0110") THEN
			ledg(8) <= '1';
			ledr(11) <= '0';
			ledg(7) <= '0';
			ledr(0) <= '1';
			next_state <= STATE2;
		 ELSIF (state2counter > "0101" and state2counter < "0111") THEN
			ledg(8) <= '0';
			ledr(11) <= TenHzModCLK;
			ledg(7) <= '0';
			ledr(0) <= '1';
			next_state <= STATE2;
		 ELSE
			ledg(8) <= '0';
			ledr(11) <= TenHzModCLK;
			ledg(7) <= '0';
			ledr(0) <= '1';		
			IF (day_state = NIGHT and def_side = NS and sw(14) = '0') THEN
				next_state <= STATE2;
			ELSE
				next_state <= STATE1;
			END IF;
		 END IF;
		 
	  WHEN STATE3=>
		 state_number <= "0011";
         IF (state3counter < "0110") THEN
			ledg(8) <= '0';
			ledr(11) <= '1';
			ledg(7) <= '1';
			ledr(0) <= '0';
			next_state <= STATE3;
		 ELSIF (state3counter > "0101" and state3counter < "0111") THEN
			ledg(8) <= '0';
			ledr(11) <= '1';
			ledg(7) <= '0';
			ledr(0) <= TenHzModCLK;
			next_state <= STATE3;
		 ELSE
			ledg(8) <= '0';
			ledr(11) <= '1';
			ledg(7) <= '0';
			ledr(0) <= TenHzModCLK;
			IF (day_state = NIGHT and def_side = EW and sw(15) = '0') THEN
				next_state <= STATE3;
			ELSE
				next_state <= STATE0;
			END IF;
		 END IF;
		 		  
	END CASE;
   END PROCESS;
----------------------------------------------------------------------------------------------------
   SeqLogic: PROCESS(MainCLK, state) -- creats sequential logic to latch the state
   BEGIN
      IF (rising_edge(MainCLK)) THEN       
         
         state <= next_state;                      -- on the rising edge of clock the current state is updated with next state
         
		 state_counter <= state_counter + 1;    -- on the rising edge of clock the current counter is incremented
		 
		 IF (state = STATE0) THEN
			IF (state0counter > "0111") THEN
				state0counter <= "0000";
			ELSE
				state0counter <= state0counter + 1;		
			END IF;

			state1counter <= "0000";
			state2counter <= "0000";
			state3counter <= "0000";
		 ELSIF (state = STATE1) THEN
			IF (state1counter > "0111") THEN
				state1counter <= "0000";
			ELSE
				state1counter <= state1counter + 1;		
			END IF;

			state0counter <= "0000";
			state2counter <= "0000";
			state3counter <= "0000";
			
		 ELSIF (state = STATE2) THEN
			IF (state2counter > "0110") THEN
				state2counter <= "0000";
			ELSE
				state2counter <= state2counter + 1;		
			END IF;
		
			state0counter <= "0000";
			state1counter <= "0000";
			state3counter <= "0000";

		 ELSIF (state = STATE3) THEN
			IF (state3counter > "0110") THEN
				state3counter <= "0000";
			ELSE
				state3counter <= state3counter + 1;		
			END IF;
				
			state0counter <= "0000";
			state1counter <= "0000";
			state2counter <= "0000";
		 END IF;
		 
		IF (state = STATE0 or state = STATE2) THEN
			wait_counter_ns <= "0000";
			wait_counter_ew <= wait_counter_ew + 1;
		ELSE
			wait_counter_ew <= "0000";
			wait_counter_ns <= wait_counter_ns + 1;	
		END IF;
      
      END IF;
      
   END PROCESS;
----------------------------------------------------------------------------------------------------
   D7S0: SevenSegment PORT MAP( state_number, '0', hex0 );
   D7S6: SevenSegment PORT MAP( std_logic_vector(wait_counter_ew), '0', hex6 );
   D7S4: SevenSegment PORT MAP( std_logic_vector(wait_counter_ns), '0', hex4 );
   D7S2: SevenSegment PORT MAP( std_logic_vector(state_counter), '0', hex2 );

END SimpleCircuit;
