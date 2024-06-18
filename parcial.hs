{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

import Text.Show.Functions
import Data.List

{-
Nombre: Montes, Fabian
Legajo: 143754-9
-}


-- PARTE A

data Usuarie = Usuarie {
    nick :: String,
    indiceFelicidad :: Double,
    librosAdquiridos :: [Libro],
    librosLeidos :: [Libro]
} deriving (Show, Eq)

carlos :: Usuarie
carlos = Usuarie {
    nick = "carloskpo",
    indiceFelicidad = 2,
    librosAdquiridos = [it, divinaComedia],
    librosLeidos = [it]
}

sofia :: Usuarie
sofia = Usuarie {
    nick = "sofitop",
    indiceFelicidad = 8,
    librosAdquiridos = [],
    librosLeidos = []
}

data Libro = Libro {
    autor :: String,
    paginas :: Double,
    genero :: Genero

} deriving (Show, Eq)

type Genero = Usuarie -> Usuarie
--type Tipo = Libro -> String

it :: Libro
it = Libro {
    autor = "stephen king",
    paginas = 100,
    genero = terror

}

divinaComedia :: Libro
divinaComedia = Libro {
    autor = "dante alighieri",
    paginas = 300,
    genero = comediaDramatica

}

{-
bladeRunner :: Libro
bladeRunner = Libro {
    autor = "dante alighieri",
    paginas = 20,
    genero = cienciaFiccion

}
-}

-- Parte B

comediaDramatica :: Genero
comediaDramatica = id


comediaAbsurda :: Genero
comediaAbsurda = aumentaFelicidad 5

aumentaFelicidad cantidad usuarie = usuarie {
    indiceFelicidad = indiceFelicidad usuarie + cantidad
}

comediaSatirica :: Genero
comediaSatirica = duplicaFelicidad

duplicaFelicidad usuarie = usuarie {
    indiceFelicidad = indiceFelicidad usuarie * 2
}

otrasComedias :: Genero
otrasComedias = aumentaFelicidad 10

cienciaFiccion :: Genero
cienciaFiccion = invierteOrden

invierteOrden usuarie = usuarie {
    nick = reverse $ nick usuarie
}


terror :: Genero
terror usuarie = usuarie {
    librosAdquiridos = []
}   -- saber si tambien se borran los leidos.


-- Parte C


leerLibro :: Usuarie -> Libro -> Usuarie
leerLibro usuarie libro = (genero libro) usuarie
leerLibro usuarie libro = usuarie {
    librosLeidos = (++) (librosLeidos usuarie) [libro]
}


sePoneAlDia :: Usuarie -> Usuarie
sePoneAlDia usuarie = foldl (leerLibro libro usuarie) usuarie (librosAdquiridos usuarie)


esFanatico :: Usuarie -> Bool
esFanatico usuarie = all ((==) (head (autoresDeLibrosLeidos usuarie))) (autoresDeLibrosLeidos usuarie)

autoresDeLibrosLeidos usuarie = autoresDeLibros (librosLeidos usuarie)

autoresDeLibros = map autor


autoresDeLibrosAdquiridos usuarie = autoresDeLibros (librosAdquiridos usuarie)

type Tipo = Libro -> String

tipo :: Tipo
tipo libro 
    | paginas libro <100 = "cuento"
    | paginas libro < 100 && paginas libro < 100 = "novela corta"
    | otherwise = "novela"
