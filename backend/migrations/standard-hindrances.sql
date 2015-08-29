DROP TABLE IF EXISTS "StandardHindrance";
CREATE TABLE IF NOT EXISTS StandardHindrance (
    "id" INTEGER ,
    "name" VARCHAR(255) NOT NULL,
    "severity" TEXT NOT NULL,
    "description" VARCHAR(255) NOT NULL,
    "createdAt" DATETIME NOT NULL,
    "updatedAt" DATETIME NOT NULL,    PRIMARY KEY ( "id" )

);
