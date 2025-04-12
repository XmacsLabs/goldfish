;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(import (liii check)
        (liii bitwise))

(check-set-mode! 'report-failed)

(check (bitwise-not 0) => -1)
(check (bitwise-not 1) => -2)
(check (bitwise-not #b1000) => -9)
(check (bitwise-not -1) => 0)

(check (bitwise-and 5 3) => 1)  ; 5 (101) AND 3 (011) = 1 (001)
(check (bitwise-and 8 4) => 0)  ; 8 (1000) AND 4 (0100) = 0 (0000)
(check (bitwise-and #b101 #b011) => 1)  ; 5 (101) AND 3 (011) = 1 (001)  
(check (bitwise-and #b1000 #b0100) => 0) ; 8 (1000) AND 4 (0100) = 0 (0000)
(check (bitwise-and #b1100 #b1010) => 8) 

(check (bitwise-ior 5 3) => 7)  ; 5 (101) OR 3 (011) = 7 (111)
(check (bitwise-or 5 3) => 7)
(check (bitwise-ior 8 4) => 12) ; 8 (1000) OR 4 (0100) = 12 (1100)
(check (bitwise-ior #b101 #b011) => 7)  ; 5 (101) AND 3 (011) = 1 (001)  
(check (bitwise-ior #b1000 #b0100) => 12) ; 8 (1000) AND 4 (0100) = 0 (0000)
(check (bitwise-ior #b1100 #b0001) => 13)

(check (bitwise-xor 1 1) => 0)
(check (bitwise-xor #b10 #b11) => #b01) ; 2 xor 3 = 1
(check (bitwise-xor #b101010 #b110100) => #b011110) ; 42 xor 20 = 34
(check (bitwise-xor #b0 #b0) => #b0) ; 0 xor 0 = 0
(check (bitwise-xor #b1 #b1) => #b0) ; 1 xor 1 = 0
(check (bitwise-xor #b101 #b111) => #b010) ; 5 xor 7 = 2
(check (bitwise-xor #b1000 #b1001) => #b0001) ; 8 xor 9 = 1
(check (bitwise-xor #b10010101 #b01111001) => #b11101100)

(check (bitwise-eqv 1 1) => #t)
(check (bitwise-eqv 1 2) => #f)
(check (bitwise-eqv -1 -1) => #t)
(check (bitwise-eqv -1 -2) => #f)
(check (bitwise-eqv 1 0) => #f)
(check (bitwise-eqv -1 0) => #f)
(check (bitwise-eqv #b1010 #b1010) => #t) ; 10 eqv 10 = #t
(check (bitwise-eqv #b1010 #b0101) => #f) ; 10 eqv 5 = #f

(check (bitwise-nor 2 4) => -7)  
(check (bitwise-nor 3 1) => -4)  
(check (bitwise-nor #b111 #b011) => -8)  
(check (bitwise-nor #b1101 #b1011) => -16) 
(check (bitwise-nor #b1100 #b0000) => -13) 

(check (bitwise-nand 1 1) => -2)  
(check (bitwise-nand 3 1) => -2)  
(check (bitwise-nand #b110 #b001) => -1)    
(check (bitwise-nand #b1001 #b0111) => -2) 
(check (bitwise-nand #b1011 #b0101) => -2) 

(check (bit-count 0) =>  0)
(check (bit-count -1) =>  0)
(check (bit-count 7) =>  3)
(check (bit-count  13) =>  3)
(check (bit-count -13) =>  2)
(check (bit-count  30) =>  4)
(check (bit-count -30) =>  4)
(check (bit-count (arithmetic-shift #b10 61)) => 1)

(check (bitwise-orc1 1 1) => -1)
(check (bitwise-orc1 3 1) => -3)
(check (bitwise-orc1 11 26) => -2)
(check (bitwise-orc1 #b110 #b001) => -7)
(check (bitwise-orc1 #b1001 #b0111) => -9)
(check (bitwise-orc1 #b1011 #b0101) => -11)

(check (bitwise-orc2 11 26) => -17)
(check (bitwise-orc2 3 1) => -1)
(check (bitwise-orc2 #b110 #b001) => -2)
(check (bitwise-orc2 #b1001 #b0111) => -7)
(check (bitwise-orc2 #b1011 #b0101) => -5)

(check (bitwise-andc1 11 26) => 16)
(check (bitwise-andc1 5 3) => 2)
(check (bitwise-andc1 #b1100 #b1010) => 2)
(check (bitwise-andc1 0 15) => 15)
(check (bitwise-andc1 15 0) => 0)
(check (bitwise-andc1 7 1) => 0)

(check (bitwise-andc2 11 26) => 1)
(check (bitwise-andc2 5 3) => 4)
(check (bitwise-andc2 #b1100 #b1010) => 4)
(check (bitwise-andc2 0 15) => 0)
(check (bitwise-andc2 15 0) => 15)
(check (bitwise-andc2 7 1) => 6)

(check (arithmetic-shift #b10 -1) => #b1) ; 2 >> 1 = 1
(check (arithmetic-shift #b10 1) => #b100) ; 2 << 1 = 4
(check (arithmetic-shift #b1000 -2) => #b10) ; 8 >> 2 = 2
(check (arithmetic-shift #b1000 2) => #b100000)
(check (arithmetic-shift #b10000000000000000 -3) => #b10000000000000)
(check (arithmetic-shift #b1000000000000000 3) => #b1000000000000000000)

(check (integer-length 0) => 0)
(check (integer-length 1) => 1)     ; 1
(check (integer-length 3) => 2)     ; 11
(check (integer-length 4) => 3)     ; 100
(check (integer-length -5) => 3)    ; -101 (长度为3)
(check (integer-length #xFFFF) => 16) ; 16位二进制

(check (bitwise-if 3 1 8) => 9)  ; #b011 #001 #100 => #101
(check (bitwise-if 3 8 1) => 0)  ; #011 #100 #001 => #000
(check (bitwise-if 1 1 2) => 3)  ; #001 #001 #010 => #011
(check (bitwise-if #b00111100 #b11110000 #b00001111) => #b00110011)  ; 60 240 15 => 51

(check-report)

