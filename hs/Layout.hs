
--
-- This is the Layout of Deploying Contract 
--
--
-- Ordinary in EVM, there is 
-- creation code and runtime code i.e. 
--
--  EVM byte code layout is 
--
--  +--------------------+ 
--  | Creation CODE      |       abondoned in deployment 
--  +--------------------+   ^---------------------------
--  |                    | 
--  |                    | 
--  | Runtime CODE       | 
--  |                    | 
--  |                    | 
--  +--------------------+ 
--
--
--  However we can add extra data in the code such that 
--  CODECOPY EVM opcode can extract the data i.e. 
--
--  +--------------------+ 
--  | Creation CODE      |       abandoned in deployment 
--  +--------------------+   ^---------------------------
--  |                    | 
--  |                    | 
--  | Runtime CODE       | 
--  |                    | 
--  |                    | 
--  +--------------------+ 
--  | Extra DATA         | 
--  +--------------------+ 
--
--  In ExtraData.hs we define the data structure of the extra data. 



module Layout where 


import ExtraData 






