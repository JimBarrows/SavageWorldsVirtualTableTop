DROP TABLE IF EXISTS "StandardEdge";
CREATE TABLE IF NOT EXISTS StandardEdge (
    "id" INTEGER ,
    "name" VARCHAR(255) NOT NULL,
    "description" VARCHAR(255) NOT NULL,
    "createdAt" DATETIME NOT NULL,
    "updatedAt" DATETIME NOT NULL,    PRIMARY KEY ( "id" )

);