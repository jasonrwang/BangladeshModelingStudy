# EPA1351 – Bangladesh Road Infrastructure Upgrade Study

The project investigates the criticality and vulnerability of various roads and bridges in Bangladesh to provide a prioritized list of infrastructure to the World Bank for upgrade projects. Collectively, this repository represents four assignments completed for the EPA1351 – Advanced Discrete Simulation course at TU Delft.

## Authors
* Hsin Cheng
* Margriet Cox
* Selma van Delft
* Jason R. Wang

## How-To

For assignment 4, ensure Simio and and MySQL can talk to each other. Then, just adjust the MySQL parameters at the top of `main.R` and run all of it, except to note which of the two types of visualization (real-time or a replay) you'd like to run. Note that both are limited by the outputs of Simio into MySQL.

![Replay Visualization GIF](https://gfycat.com/MeagerSlipperyGnat.gif)

### Debugging

Checklist of problems we ran into:

#### MySQL 

-[ ] Is the MySQL `bind-address` statement set, especially from a HomeBrew environment, so MySQL broadcasts on/listens to the network?
-[ ] Check the user permissions for the intended user group – should be `user@%` as a wildcard or explicitly defined for the client IP
-[ ] Flush privileges and restart the server if it isn't running!

#### Simio

-[ ] Is the dbConnect1 process set to the correct network parameters?
-[ ] Is the NET Connector installed?

#### R

-[ ] If there is a database connect error, re-run dbConnect.
-[ ] Are the dbConnect parameters correct (especially db_table)?
-[ ] Are the data file headers set correctly? 'LRP' and 'lrp' change quite often!
