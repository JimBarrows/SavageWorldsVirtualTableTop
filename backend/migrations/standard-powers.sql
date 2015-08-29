DROP TABLE IF EXISTS "StandardPower";
CREATE TABLE IF NOT EXISTS StandardPower (
    "id" INTEGER ,
    "name" VARCHAR(255) NOT NULL,
    "description" VARCHAR(255) NOT NULL,
    "rank" VARCHAR(255) NOT NULL,
    "powerPoints" INTEGER NOT NULL,
    "range" VARCHAR(255) ,
    "duration" INTEGER NOT NULL,
    "maintenanceCost" INTEGER NOT NULL,
    "createdAt" DATETIME NOT NULL,
    "updatedAt" DATETIME NOT NULL,    PRIMARY KEY ( "id" )

);
