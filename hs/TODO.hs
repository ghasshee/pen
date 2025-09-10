-- TODO.hs 
--
--
--
-- this file contains what tasks should we do the next or in the follwoing steps 
--
--



-- Overview of the directory

-- Kripke.hs    -- defines Kripke Structure which is used in LTL analysis
--
--
--
-- Datatype.hs  -- we have to define not only structures of datatype but also semantics
--              e.g. List has head/tail which are semantics while cons/nil is syntax
--
--
--
-- #TODO Parser.y  -- removal of LET Sugar Syntax 
--
--
--
--
--
-- The semiring automaton graph : it's the program
-- duplicate them into two worlds, A and B. 
-- we start from the world A and if we satisfied required condition, 
-- we move to the other world B. 
--
-- If we have two required conditions, 
-- we go from A to B, then we go from B1 to B2. 
-- if two conditions are ordered, then we cannot go from A1 through A2 to B2. 
-- That is implemented by Kripke Possible World using Temporal Logical statements. 
--
--
--
