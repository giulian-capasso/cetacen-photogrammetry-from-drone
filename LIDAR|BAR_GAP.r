#### ----------------------------------- LIDAR|BAR GAP ------------------------------------- ####
# ---------------------------------------- Function ------------------------------------------- #

GAP <- function(data, col1, col2, start_index, end_index) {
  # Estrai le colonne di interesse nel range specificato
  subset_data <- data.frame(
    col1 = data[[col1]][start_index:end_index],
    col2 = data[[col2]][start_index:end_index]
  )
  
  # Calcola la differenza riga per riga
  differences <- subset_data$col1 - subset_data$col2
  
  # Rimuovi le differenze con NA
  valid_differences <- differences[!is.na(differences)]
  
  # Calcola media, moda e mediana delle differenze
  mean_diff <- mean(valid_differences, na.rm = TRUE)
  
  # Calcola la moda solo se ci sono differenze valide
  mode_diff <- ifelse(length(valid_differences) > 0, as.numeric(names(table(valid_differences))[which.max(table(valid_differences))]), NA)
  
  median_diff <- median(valid_differences, na.rm = TRUE)
  
  # Restituisci i risultati
  return(list(mean_diff = mean_diff, mode_diff = mode_diff, median_diff = median_diff))
}

# Esempio di utilizzo della funzione
GAP(FLY297_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 287, 300)

#### ----------------------------------- Analysis ------------------------------------- ####


# FLY 297 + LOG0027 
FLY297_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_297
GAP_297 <- GAP(FLY297_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 287, 300)
GAP_297

# FLY 298 + LOG0028
FLY298_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_298
GAP_298 <- GAP(FLY298_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 100, 550)
GAP_298

# FLY 299 + LOG0029
FLY299_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_299
GAP_299 <- GAP(FLY299_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 65, 680)
GAP_299

# FLY 304 + LOG0030
FLY304_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_304
GAP_304 <- GAP(FLY304_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 265, 925)
GAP_304

# FLY 306 + LOG0031
FLY306_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_306
GAP_306 <- GAP(FLY306_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 61, 340)
GAP_306

# FLY 307 + LOG0032
FLY307_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_307
GAP_307 <- GAP(FLY307_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 50, 620)
GAP_307

# FLY 310 + LOG0034
FLY310_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_310
GAP_310 <- GAP(FLY310_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 60, 600)
GAP_310

# FLY 311 + LOG0035
FLY311_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_311
GAP_311 <- GAP(FLY311_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 60, 470)
GAP_311

# FLY 312 + LOG0036
FLY312_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_312
GAP_312 <- GAP(FLY312_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 85, 225)
GAP_312

# FLY 313 + LOG0037 # errore su moda
FLY313_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_313
GAP_313 <- GAP(FLY313_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 80, 280)
GAP_313

# FLY 315 + LOG0038
FLY315_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_315
GAP_315 <- GAP(FLY315_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 170, 435)
GAP_315

# FLY 316 + LOG0039 decollo laser 1.92m
FLY316_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_316
GAP_316 <- GAP(FLY316_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 125, 195)
GAP_316

# FLY 317 + LOG0040
FLY317_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_317
GAP_317 <- GAP(FLY317_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 80, 430)
GAP_317

# FLY 318 + LOG0041 (decollo da 3.125BAR e 7.025833LASER) ( se sottraiamo 3 alla mediana di 8 torniamo a 5)
FLY318_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_318
GAP_318 <- GAP(FLY318_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 55, 200)
GAP_318

# FLY 319 + LOG0042
FLY319_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_319
GAP_319 <- GAP(FLY319_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 156, 290)
GAP_319

# FLY 323 + LOG0045
FLY323_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_323
GAP_323 <- GAP(FLY323_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 345, 440)
GAP_323

# FLY 327 + LOG0048
FLY327_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_327
GAP_327 <- GAP(FLY327_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 86, 140)
GAP_327

# FLY 330 + LOG0050
FLY330_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_330
GAP_330 <- GAP(FLY330_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 70, 750)
GAP_330

# FLY 332 + LOG0052
FLY332_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_332
GAP_330 <- GAP(FLY330_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 190, 325)
GAP_330

# FLY 352 + LOG0068
FLY352_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_352
GAP_352 <- GAP(FLY352_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 390, 525)
GAP_352

# FLY 354 + LOG0070
FLY354_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_354
GAP_354 <- GAP(FLY354_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 55, 470)
GAP_354

# FLY 365 + LOG0071
FLY365_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_365
GAP_365 <- GAP(FLY365_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 100, 690)
GAP_365

# FLY 368 + LOG0064
FLY368_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned
clean_368
GAP_368 <- GAP(FLY368_REC_MOT_LD_1.2_clean, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 53, 600)
GAP_368

# FLY 371 + LOG0075
FLY371_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_371
GAP_371 <- GAP(FLY371_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 170, 300)
GAP_371

# FLY 372 + LOG0076 #da capire com'è andato questo volo, 8 metri di differenza
FLY372_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_372
GAP_372 <- GAP(FLY372_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 50, 140)
GAP_372

# FLY 374 + LOG0078
FLY374_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_374
GAP_374 <- GAP(FLY374_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 918, 990)
GAP_374

# FLY 382 + LOG0079
FLY382_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_382
GAP_382 <- GAP(FLY382_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 300, 430)
GAP_382

# FLY 383 + LOG0080
FLY383_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_382
GAP_383 <- GAP(FLY383_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 100, 530)
GAP_383

# FLY 384 + LOG0081
FLY384_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_384
GAP_384 <- GAP(FLY384_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 180, 680)
GAP_384

# FLY 387 + LOG0084
FLY387_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_387
GAP_387 <- GAP(FLY387_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 190, 240)
GAP_387

# FLY 388 + LOG0085 #da capire com'è andato questo volo, 8 metri di differenza
FLY388_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_388
GAP_388 <- GAP(FLY388_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 33, 57)
GAP_388

# FLY 389 + LOG0086 #da capire com'è andato questo volo, 2.7 metri di differenza
FLY389_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned
clean_389
GAP_389 <- GAP(FLY389_REC_MOT_LD_1.2_clean, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 733, 800)
GAP_389

# FLY 393 + LOG0088 # volo a terra
FLY393_REC_MOT_LD_1.2$laser_altitude_m
plot_393
GAP_393 <- GAP(FLY393_REC_MOT_LD_1.2, "laser_altitude_m", "osd_data:relativeHeight[meters]", 20, 220)
GAP_393

FLY393_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]` <- c(0, 0, 0, head(FLY393_REC_MOT_LD_1.2$`osd_data:relativeHeight[meters]`, -3))

#Spostare il drone di 3 in avanti per far matchare !!!! (?) da capire meglio

# FLY 396 + LOG0090
FLY396_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned
clean_396
GAP_396 <- GAP(FLY396_REC_MOT_LD_1.2_clean, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 375, 600)
GAP_396

# FLY 400 + LOG0093
FLY400_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_400
GAP_400 <- GAP(FLY400_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 200, 310)
GAP_400

# FLY 401 + LOG0094
FLY401_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_401
GAP_401 <- GAP(FLY401_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 560, 580)
GAP_401

# FLY 402 + LOG0095
FLY402_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_402
GAP_402 <- GAP(FLY402_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 80, 200)
GAP_402

# FLY 411 + LOG0099
FLY411_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_411
GAP_411 <- GAP(FLY411_REC_MOT_LD_1.2, "laser_altitude_m", "osd_data:relativeHeight[meters]", 35, 310)
GAP_411

# FLY 412 + LOG0100
FLY414_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_414
GAP_414 <- GAP(FLY414_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 200, 375)
GAP_414

# FLY 414 + LOG0103
FLY414_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_414
GAP_414 <- GAP(FLY414_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 200, 375)
GAP_414

# FLY 417 + LOG0106
FLY417_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_417
GAP_417 <- GAP(FLY417_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 220, 790)
GAP_417

# FLY 420 + LOG0108
FLY420_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned2
clean_420
GAP_420 <- GAP(FLY420_REC_MOT_LD_1.2_clean, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 70, 480)
GAP_420

# FLY 421 + LOG0109
FLY421_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_421
GAP_421 <- GAP(FLY421_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 70, 270)
GAP_421

# FLY 423 + LOG0112
FLY423_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_423
GAP_423 <- GAP(FLY423_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 50, 140)
GAP_423

# FLY 424 + LOG0113
FLY424_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_424
GAP_424 <- GAP(FLY424_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 140, 240)
GAP_424

# FLY 425 + LOG0115
FLY425_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_425
GAP_425 <- GAP(FLY425_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 90, 450)
GAP_425

# FLY 427 + LOG0117
FLY427_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_427
GAP_427 <- GAP(FLY427_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 400, 650)
GAP_427

# FLY 427 + LOG0117
FLY429_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_429
GAP_429 <- GAP(FLY429_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 60, 155)
GAP_429

# FLY 430 + LOG0125
FLY430_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned
clean_430
GAP_430 <- GAP(FLY430_REC_MOT_LD_1.2_clean, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 290, 490)
GAP_430

# FLY 431 + LOG0127
FLY431_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_431
GAP_431 <- GAP(FLY431_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 315, 550)
GAP_431

# FLY 434 + LOG0129
FLY434_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_434
GAP_434 <- GAP(FLY434_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 180, 650)
GAP_434

# FLY 436 + LOG0131
FLY436_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_436
GAP_436 <- GAP(FLY436_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 60, 310)
GAP_436

# FLY 437 + LOG0132
FLY437_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_437
GAP_437 <- GAP(FLY437_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 130, 350)
GAP_437

# FLY 438 + LOG0133
FLY438_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_438
GAP_438 <- GAP(FLY438_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 100, 310)
GAP_438

# FLY 440 + LOG0134
FLY440_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_440
GAP_440 <- GAP(FLY440_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 250, 850)
GAP_440

# FLY 444 + LOG0138
FLY444_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_444
GAP_444 <- GAP(FLY444_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 70, 515)
GAP_444

# FLY 446 + LOG0139
FLY446_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_446
GAP_446 <- GAP(FLY446_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 90, 240)
GAP_446

# FLY 447 + LOG0140
FLY447_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_447
GAP_447 <- GAP(FLY447_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 120, 250)
GAP_447

# FLY 452 + LOG0144
FLY452_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_452
GAP_452 <- GAP(FLY452_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 1, 315)
GAP_452

# FLY 456 + LOG0148
FLY456_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_456
GAP_456 <- GAP(FLY456_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 51, 200)
GAP_456

# FLY 457 + LOG0149
FLY457_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_457
GAP_457 <- GAP(FLY457_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 70, 140)
GAP_457

# FLY 458 + LOG0150
FLY458_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_458
GAP_458 <- GAP(FLY458_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 630, 750)
GAP_458

# FLY 459 + LOG0151
FLY459_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_459
GAP_459 <- GAP(FLY459_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 100, 460)
GAP_459

# FLY 461 + LOG0152
FLY461_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_461
GAP_461 <- GAP(FLY461_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 50, 400)
GAP_461

# FLY 462 + LOG0153 # 8 m di distanza capire com'è andato il volo
FLY462_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_462
GAP_462 <- GAP(FLY462_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 18, 188)
GAP_462

# FLY 463 + LOG0154
FLY463_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_463
GAP_463 <- GAP(FLY463_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 200, 600)
GAP_463

# FLY 464 + LOG0155
FLY464_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_464
GAP_464 <- GAP(FLY464_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 120, 545)
GAP_464

# FLY 466 + LOG0157
FLY466_REC_MOT_LD_1.2_clean$laser_altitude_m_cleaned
clean_466
GAP_466 <- GAP(FLY466_REC_MOT_LD_1.2_clean, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 62, 78)
GAP_466

# FLY 472 + LOG0161
FLY472_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_472
GAP_472 <- GAP(FLY472_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 460, 920)
GAP_472

# FLY 475 + LOG0164
FLY475_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_475
GAP_475 <- GAP(FLY475_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 220, 350)
GAP_475

# FLY 477 + LOG0166
FLY477_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_477
GAP_477 <- GAP(FLY477_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 275, 450)
GAP_477

# FLY 478 + LOG0167
FLY478_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_478
GAP_478 <- GAP(FLY478_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 170, 370)
GAP_478

# FLY 484 + LOG0168
FLY484_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_484
GAP_484 <- GAP(FLY484_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 50, 420)
GAP_484

# FLY 487 + LOG0170
FLY487_REC_MOT_LD_1.2$laser_altitude_m_cleaned
clean_487
GAP_487 <- GAP(FLY487_REC_MOT_LD_1.2, "laser_altitude_m_cleaned", "osd_data:relativeHeight[meters]", 135, 400)
GAP_487


#### ------------------------------------------------ Stats ------------------------------------------------ ####

overall_stats <- function(gap_list) {
  # Estrai le differenze, medie, mode, mediane e deviazioni standard
  mean_diffs <- sapply(gap_list, function(gap) gap$mean_diff)
  mode_diffs <- sapply(gap_list, function(gap) gap$mode_diff)
  median_diffs <- sapply(gap_list, function(gap) gap$median_diff)
  
  # Filtra i valori non validi
  valid_mean_diffs <- mean_diffs[!is.na(mean_diffs)]
  valid_mode_diffs <- mode_diffs[!is.na(mode_diffs)]
  valid_median_diffs <- median_diffs[!is.na(median_diffs)]
  
  # Calcola le deviazioni standard solo se ci sono valori validi
  overall_sd_mean_diff <- if (length(valid_mean_diffs) > 0) sd(valid_mean_diffs) else NA
  overall_sd_mode_diff <- if (length(valid_mode_diffs) > 0) sd(valid_mode_diffs) else NA
  overall_sd_median_diff <- if (length(valid_median_diffs) > 0) sd(valid_median_diffs) else NA
  
  # Calcola le medie solo se ci sono valori validi
  overall_mean_mean_diff <- if (length(valid_mean_diffs) > 0) mean(valid_mean_diffs) else NA
  overall_mean_mode_diff <- if (length(valid_mode_diffs) > 0) mean(valid_mode_diffs) else NA
  overall_mean_median_diff <- if (length(valid_median_diffs) > 0) mean(valid_median_diffs) else NA
  
  # Calcola il minimo e il massimo
  min_mean_diff <- if (length(valid_mean_diffs) > 0) min(valid_mean_diffs) else NA
  max_mean_diff <- if (length(valid_mean_diffs) > 0) max(valid_mean_diffs) else NA
  
  min_median_diff <- if (length(valid_median_diffs) > 0) min(valid_median_diffs) else NA
  max_median_diff <- if (length(valid_median_diffs) > 0) max(valid_median_diffs) else NA
  
  # Restituisci i risultati
  return(list(
    overall_sd_mean_diff = overall_sd_mean_diff,
    overall_sd_mode_diff = overall_sd_mode_diff,
    overall_sd_median_diff = overall_sd_median_diff,
    overall_mean_mean_diff = overall_mean_mean_diff,
    overall_mean_mode_diff = overall_mean_mode_diff,
    overall_mean_median_diff = overall_mean_median_diff,
    min_mean_diff = min_mean_diff,
    max_mean_diff = max_mean_diff,
    min_median_diff = min_median_diff,
    max_median_diff = max_median_diff
  ))
}

# Esempio di utilizzo:
gap_list <- list(GAP_298, GAP_299, GAP_304, GAP_306, GAP_307, GAP_310, GAP_311, GAP_312, GAP_313, GAP_315,
                 GAP_316, GAP_317, GAP_318, GAP_319, GAP_323, GAP_327, GAP_330, GAP_352, GAP_353, GAP_354,
                 GAP_365, GAP_368, GAP_371, GAP_372, GAP_374, GAP_382, GAP_383, GAP_384, GAP_387, GAP_388,
                 GAP_389, GAP_393, GAP_396, GAP_400, GAP_401, GAP_402, GAP_411, GAP_412, GAP_414, GAP_417,
                 GAP_420, GAP_421, GAP_423, GAP_424, GAP_425, GAP_427, GAP_429, GAP_430, GAP_431, GAP_434,
                 GAP_436, GAP_437, GAP_438, GAP_440, GAP_444, GAP_446, GAP_447, GAP_452, GAP_456, GAP_457,
                 GAP_458, GAP_459, GAP_461, GAP_462, GAP_463, GAP_464, GAP_466, GAP_472, GAP_475, GAP_477,
                 GAP_478, GAP_484, GAP_487)

overall_stats(gap_list)
