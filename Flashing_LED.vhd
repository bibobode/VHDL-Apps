LIBRARY ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.ALL;

-- The above libaries lines must be included in every VHDL file, before EVERY ENTITY!

--
-- This is the main circuit Entity, which connects all wires to the FPGA pins (lights and switches)
-- First we have a PORT mapping - naming all wires going to the outside world and if they are INputs or OUTputs
--   

ENTITY Lab1 IS PORT(
      
-- Note that all signal names here are fixed by the "DE2_pins.csv" file which you must use for every lab

      key   : IN  std_logic_vector(3 downto 0); -- 4 push buttons on the board - normally HIGH or '1' when not pressed
      sw    : IN  std_logic_vector(1 downto 0); -- use 6 out of 18 switches on the board - '0' (LOW) when down towards edge of board

      ledr  : OUT std_logic_vector(1 downto 0); -- Red LEDs, only 2 used
      ledg  : OUT std_logic_vector(1 downto 0)  -- Green LEDs, only 2 used
);
END Lab1;

ARCHITECTURE SimpleCircuit OF Lab1 IS

--
-- Create any signals, or temporary variables, to be used
--
-- Signals are either a vector or not.  A vector is a group of two or more signals
--
-- Note that there are two basic types and we nearly always use std_logic:
-- UNSIGNED is a signal which can be used to perform math operations such as +, -, *
-- std_logic_vector is a signal which can be used for logic operations such as OR, AND, NOT, XOR
--

signal input_a, input_b: std_logic_vector(3 downto 0); -- two signals for inputs
signal red_led0, red_led1: std_logic;  -- two signals for Red LED outputs
signal green_led0, green_led1: std_logic;  -- two signals for Green LED outputs

-- The function of Lab1 entity is defined here

BEGIN

   
   ledr(0) <= red_led0;    -- set ledr(0) and ledr(1) to intermediate signals
   ledr(1) <= red_led1;    -- the red_led1 multiplexer is commented
  
   ledg(0) <= green_led0;   
   ledg(1) <= green_led1;    

   
	input_a<= key(2 downto 0) & sw(1);
   with input_a select                
        green_led0 <= '0' when "0000",   
                      '0' when "0010",
                      '0' when "0100",
                      '0' when "1000",
                      '0' when "0110",
                      '0' when "1010",
                      '1' when "1100",
                      '0' when "1110",
                      
                      '0' when "0001",   
                      '0' when "0011",
                      '0' when "0101",
                      '0' when "1001",
                      '0' when "0111",
                      '0' when "1011",
                      '0' when "1101",
                      '0' when "1111";
                      
   with input_a select                     
        red_led0 <= '1' when "0000",   
                    '1' when "0010",
                    '1' when "0100",
                    '0' when "1000",
                    '1' when "0110",
                    '0' when "1010",
                    '0' when "1100",
                    '0' when "1110",
                    
                    '1' when "0001",   
                    '1' when "0011",
                    '1' when "0101",
                    '1' when "1001",
                    '1' when "0111",
                    '1' when "1011",
                    '1' when "1101",
                    '1' when "1111";
   
   
END SimpleCircuit;
