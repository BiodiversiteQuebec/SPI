library(sf)
source("scr/01-run_SPI_computation.r")

# Get array number from command line
# rm(list = ls())
ARRAY_ID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

#------------------------------------------------------------------------------
# 0. PARAMS
#------------------------------------------------------------------------------
PROTECTED_AREA_TYPE = "" # Types of protected areas to consider (unique(aires_prot$DESIG_GR))
SPLIT = TRUE # Split computations into total, south and north regions
UNION = TRUE # Union of protected areas ?
YEARS_LIST <- c(1876, 1900, 1919, 1925, 1927, 1931, 1937, 1938, 1941, 1955, 1960, 1970, 1972, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)
SPECIES_LIST <- c("Acipenser fulvescens", "Acipenser oxyrinchus", "Alces americanus", 
    "Alosa aestivalis", "Alosa pseudoharengus", "Alosa sapidissima", 
    "Ambloplites rupestris", "Ambystoma laterale", "Ambystoma maculatum", 
    "Ameiurus natalis", "Ameiurus nebulosus", "Amia calva", "Ammocrypta pellucida", 
    "Anaxyrus americanus", "Anguilla rostrata", "Apalone spinifera", 
    "Apeltes quadracus", "Aplodinotus grunniens", "Blarina brevicauda", 
    "Canis latrans", "Canis lupus", "Carassius auratus", "Carpiodes cyprinus", 
    "Castor canadensis", "Catostomus catostomus", "Catostomus commersonii", 
    "Chelydra serpentina", "Chrosomus eos", "Chrosomus neogaeus", 
    "Chrysemys picta", "Clemmys guttata", "Condylura cristata", "Coregonus artedi", 
    "Coregonus clupeaformis", "Cottus bairdii", "Cottus cognatus", 
    "Cottus ricei", "Couesius plumbeus", "Ctenopharyngodon idella", 
    "Culaea inconstans", "Cyprinella spiloptera", "Cyprinus carpio", 
    "Dermochelys coriacea", "Desmognathus fuscus", "Desmognathus ochrophaeus", 
    "Diadophis punctatus", "Dicrostonyx hudsonius", "Didelphis virginiana", 
    "Dorosoma cepedianum", "Emydoidea blandingii", "Eptesicus fuscus", 
    "Erethizon dorsatum", "Esox americanus americanus", "Esox americanus vermiculatus", 
    "Esox lucius", "Esox masquinongy", "Esox niger", "Etheostoma exile", 
    "Etheostoma flabellare", "Etheostoma nigrum", "Etheostoma olmstedi", 
    "Eurycea bislineata", "Exoglossum maxillingua", "Fundulus diaphanus", 
    "Fundulus heteroclitus", "Gasterosteus aculeatus", "Gasterosteus wheatlandi", 
    "Glaucomys sabrinus", "Glaucomys volans", "Glyptemys insculpta", 
    "Graptemys geographica", "Gulo gulo", "Gyrinophilus porphyriticus", 
    "Hemidactylium scutatum", "Hiodon alosoides", "Hiodon tergisus", 
    "Hybognathus hankinsoni", "Hybognathus regius", "Hyla versicolor", 
    "Ichthyomyzon castaneus", "Ichthyomyzon fossor", "Ichthyomyzon unicuspis", 
    "Ictalurus punctatus", "Labidesthes sicculus", "Lampropeltis triangulum", 
    "Lasionycteris noctivagans", "Lasiurus borealis", "Lasiurus cinereus", 
    "Lepisosteus osseus", "Lepomis cyanellus", "Lepomis gibbosus", 
    "Lepomis macrochirus", "Lepomis peltastes", "Lepus americanus", 
    "Lepus arcticus", "Lethenteron appendix", "Liochlorophis vernalis", 
    "Lithobates catesbeianus", "Lithobates clamitans", "Lithobates palustris", 
    "Lithobates pipiens", "Lithobates septentrionalis", "Lithobates sylvaticus", 
    "Lontra canadensis", "Lota lota", "Luxilus cornutus", "Lyn rufus", 
    "Lynx canadensis", "Margariscus margarita", "Marmota monax", 
    "Martes americana", "Martes pennanti", "Mephiti mephitis", "Microgadus tomcod", 
    "Micropterus dolomieu", "Micropterus salmoides", "Microtus chrotorrhinus", 
    "Microtus pennsylvanicus", "Microtus pinetorum", "Morone americana", 
    "Morone chrysops", "Morone saxatilis", "Moxostoma anisurum", 
    "Moxostoma carinatum", "Moxostoma hubbsi", "Moxostoma macrolepidotum", 
    "Moxostoma valenciennesi", "Mus musculus", "Mustela erminea", 
    "Mustela frenata", "Mustela nivalis", "Myodes gapperi", "Myotis leibii", 
    "Myotis lucifugus", "Myotis septentrionalis", "Myoxocephalus quadricornis", 
    "Myoxocephalus thompsonii", "Napaeozapus insignis", "Necturus maculosus", 
    "Neogobius melanostomus", "Neotamias minimus", "Neovison vison", 
    "Nerodia sipedon", "Notemigonus crysoleucas", "Notophthalmus viridescens", 
    "Notropis atherinoides", "Notropis bifrenatus", "Notropis heterodon", 
    "Notropis heterolepis", "Notropis hudsonius", "Notropis rubellus", 
    "Notropis stramineus", "Notropis volucellus", "Noturus flavus", 
    "Noturus gyrinus", "Noturus insignis", "Odocoileus virginianus", 
    "Oncorhynchus clarkii", "Oncorhynchus kisutch", "Oncorhynchus mykiss", 
    "Oncorhynchus nerka", "Oncorhynchus tshawytscha", "Ondatra zibethicus", 
    "Osmerus mordax", "Ovibos moschatus", "Parascalops breweri", 
    "Perca flavescens", "Percina caprodes", "Percina copelandi", 
    "Percopsis omiscomaycus", "Perimyotis subflavus", "Peromyscus leucopus", 
    "Peromyscus maniculatus", "Petromyzon marinus", "Phenacomys ungava", 
    "Pimephales notatus", "Pimephales promelas", "Plethodon cinereus", 
    "Pomoxis nigromaculatus", "Procyon lotor", "Prosopium cylindraceum", 
    "Pseudacris crucifer", "Pseudacris maculata", "Pseudacris triseriata", 
    "Pungitius pungitius", "Rangifer tarandus caribou", "Rattus norvegicus", 
    "Rhinichthys atratulus", "Rhinichthys cataractae", "Salmo salar", 
    "Salmo trutta", "Salvelinus alpinus", "Salvelinus fontinalis", 
    "Salvelinus namaycush", "Sander canadensis", "Sander vitreus", 
    "Scardinius erythrophthalmus", "Sciurus carolinensis", "Semotilus atromaculatus", 
    "Semotilus corporalis", "Sorex arcticus", "Sorex cinereus", "Sorex dispar", 
    "Sorex fumeus", "Sorex gaspensis", "Sorex hoyi", "Sorex palustris", 
    "Sternotherus odoratus", "Storeria dekayi", "Storeria occipitomaculata", 
    "Sylvilagus floridanus", "Synaptomys borealis", "Synaptomys cooperi", 
    "Tamias striatus", "Tamiasciurus hudsonicus", "Thamnophis sauritus", 
    "Thamnophis sirtalis", "Tinca tinca", "Umbra limi", "Urocyon cinereoargenteus", 
    "Ursus americanus", "Ursus maritimus", "Vulpes lagopus", "Vulpes vulpes", 
    "Zapus hudsonius")

#------------------------------------------------------------------------------
# 1. Select species and years
#------------------------------------------------------------------------------
sp_name <- SPECIES_LIST[ARRAY_ID]


#------------------------------------------------------------------------------
# 2. Compute SPI
#------------------------------------------------------------------------------
SPI <- run_SPI_computation(sp_name, YEARS_LIST, SPLIT = TRUE, PROTECTED_AREA_TYPE, UNION)


#------------------------------------------------------------------------------
# 2. Save SPI
#------------------------------------------------------------------------------
write.csv(SPI, paste0("results/", gsub(" ", "_", sp_name), ".csv"))
