DROP TABLE IF EXISTS "StandardGear";
CREATE TABLE IF NOT EXISTS StandardGear (
    "id" INTEGER ,
    "name" VARCHAR(255) NOT NULL,
    "description" VARCHAR(255) NOT NULL,
    "era" VARCHAR(255) NOT NULL,
    "weight" INTEGER NOT NULL,
    "cost" INTEGER NOT NULL,
    "subType" VARCHAR(255) NOT NULL,
    "notes" VARCHAR(255) NOT NULL,
    "createdAt" DATETIME NOT NULL,
    "updatedAt" DATETIME NOT NULL,    PRIMARY KEY ( "id" )

);
