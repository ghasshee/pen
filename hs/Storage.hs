module Storage where 






-- (* The Storage in Runtime                                        *)
-- (* Array elements are placed as in Solidity                      *)
-- (*                                                               *)
-- (*  S[0]  := PROGRAM COUNTER                                     *)
-- (*  S[1]  := ARRAY SEED COUNTER                                  *)
-- (*  S[2]  := pod cntrct arg0   --+                ---+           *)
-- (*   ...                         |   k  args         | n args    *)
-- (*  S[k+1]:= pod cntrct argk-1 --+                   |           *)
-- (*  S[k+2]:= array0's seed     --+                   |           *)
-- (*   ...                         | (n-k) arrSeeds    |           *)
-- (*  S[n+1]:= arraym's seed     --+                ---+           *)
