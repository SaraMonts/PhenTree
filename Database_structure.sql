-- Comanda utilitzada per a crear la taula "pedigrees": taula que recull totes les característiques de cada pedigree
CREATE TABLE pedigrees (
  ped_id 	    	  	INT NOT NULL,              -- ID del pedigree
  ped_name	 	  		VARCHAR(50),               -- Nom del pedigree
  username			    VARCHAR(32) NOT NULL,      -- Usuari que ha creat el pedigree
  creation_date	  		DATE NOT NULL,             -- Data de creació del pedigree
  n_phen			    INT NOT NULL,              -- Nombre de fenotips que es volen representar
  n_info		    	INT NOT NULL,              -- Nombre de camps d'informació que es volen afegir
  phen1_hpo	  	 	  	CHAR(10),                  -- Terme HPO del primer fenotip
  phen2_hpo		        CHAR(10),                  -- Terme HPO del segon fenotip
  phen3_hpo		  	  	CHAR(10),                  -- Terme HPO del tercer fenotip
  phen4_hpo	  			CHAR(10),                  -- Terme HPO del quart fenotip
  width		      	    REAL NOT NULL,             -- Amplada del gràfic
  height		 	    REAL NOT NULL,             -- Alçada del gràfic
  symbolsize  		  	REAL NOT NULL,             -- Mida dels símbols
  textsize		        REAL NOT NULL,             -- Mida del text
  distance		  	   	REAL NOT NULL,             -- Distància entre els símbols i el text
  pconnect	  	  		REAL NOT NULL,             -- Punt de connexió entre pares i fills
  branch		        REAL NOT NULL,             -- Inclinació de la línia de connexió entre pares i fills
  phen1_color		    CHAR(7) NOT NULL,          -- Color del primer fenotip
  phen1_density	  		INT NOT NULL,              -- Densitat del primer fenotip
  phen1_angle		  	INT NOT NULL,              -- Angle del primer fenotip
  phen2_color		    CHAR(7) NOT NULL,          -- Color del segon fenotip
  phen2_density	  		INT NOT NULL,              -- Densitat del segon fenotip
  phen2_angle		  	INT NOT NULL,              -- Angle del segon fenotip
  phen3_color		    CHAR(7) NOT NULL,          -- Color del tercer fenotip
  phen3_density	  		INT NOT NULL,              -- Densitat del tercer fenotip
  phen3_angle		  	INT NOT NULL,              -- Angle del tercer fenotip
  phen4_color			CHAR(7) NOT NULL,          -- Color del quart fenotip
  phen4_density	  		INT NOT NULL,              -- Densitat del quart fenotip
  phen4_angle	  		INT NOT NULL,              -- Angle del quart fenotip
  include1			    VARCHAR(50),               -- Variables a incloure en el gràfic 1
  include2			  	VARCHAR(50),               -- Variables a incloure en el gràfic 2
  show_legend		  	BOOLEAN NOT NULL,          -- Mostrar la llegenda?
  legend_width 		  	REAL NOT NULL,             -- Amplada de la llegenda
  legend_height	  		REAL NOT NULL,             -- Alçada de la llegenda
  legend_symbolsize		REAL NOT NULL,             -- Mida dels símbols de la llegenda
  legend_textsize		REAL NOT NULL,             -- Mida del text de la llegenda
  PRIMARY KEY (ped_id)                             -- CLAU PRIMÀRIA: el ID del pedigree
);



-- Comanda utilitzada per a crear la taula "members": taula que recull la informació de cada individu introduit en cada pedigree
CREATE TABLE members (
  ped		      	INT NOT NULL,           -- ID del pedigree
  id		      	INT NOT NULL,           -- ID de l'individu
  name			    VARCHAR(50),            -- Nom de l'individu
  sex	      		INT NOT NULL,           -- Sexe
  status    		INT NOT NULL,           -- Status
  mother		    INT,                    -- ID de la seva mare
  father    		INT,                    -- ID del seu pare
  adopted	    	VARCHAR(5),             -- Indicador de si és adoptat
  mono_twins	    INT,                    -- ID del bessó monozigòtic
  di_twins	        INT,                    -- ID del bessó dizigòtic
  unk_twins  		INT,                    -- ID del bessó de zigositat desconeguda
  marriage		    INT,                    -- ID de la parella si no tenen fills
  phen1		    	INT NOT NULL,           -- Afectació pel primer fenotip
  phen2 	    	INT NOT NULL,           -- Afectació pel segon fenotip
  phen3   			INT NOT NULL,           -- Afectació pel tercer fenotip
  phen4			    INT NOT NULL,           -- Afectació pel quart fenotip
  age	      		VARCHAR(30),            -- Edat
  consultand	    BOOLEAN NOT NULL,       -- És la persona que consulta?
  number	    	CHAR(1) NOT NULL,       -- Nombre de persones representades amb aquest símbol
  info1		    	VARCHAR(100),           -- Camp d'informació 1
  info2	    		VARCHAR(100),           -- Camp d'informació 2
  info3			    VARCHAR(100),           -- Camp d'informació 3
  info4		    	VARCHAR(100),           -- Camp d'informació 4
  tab_order		    INT NOT NULL,           -- Ordre en que s'ha de mostrar en la taula
  PRIMARY KEY (id),                         -- CLAU PRIMÀRIA: ID de l'individu
  FOREIGN KEY (ped) REFERENCES pedigrees(ped_id)       -- CLAU FORANA: ID del pedigree
);













