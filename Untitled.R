library(tidyverse)

album1 <- Compu_album_1
album2 <- compu_album_2
album3 <- Compu_album_3
album4 <- Compu_album_4

album1$Album <- "Harry Styles"
album2$Album <- "Fine Line"
album3$Album <- "Harry's House"
album4$Album <- "KATTDO"

combined_data <- bind_rows(album1, album2, album3, album4)

ggplot(combined_data, aes(x = Album, y = Energy, fill = Album)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Comparison of Energy Across Albums",
       y = "Energy",
       x = "Album")


library(tidyverse)

album1 <- Compu_album_1
album2 <- compu_album_2
album3 <- Compu_album_3
album4 <- Compu_album_4

album1$Album <- "Harry Styles"
album2$Album <- "Fine Line"
album3$Album <- "Harry's House"
album4$Album <- "KATTDO"

combined_data <- bind_rows(album1, album2, album3, album4)

ggplot(combined_data, aes(x = Album, y = Energy, fill = Album)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Harry Styles" = "#1f77b4",
    "Fine Line" = "#ff7f0e",
    "Harry's House" = "#2ca02c",
    "KATTDO" = "#d62728"
  )) +
  theme_minimal() +
  labs(title = "Comparison of Energy Across Albums",
       y = "Energy",
       x = "Album")

library(tidyverse)

album1 <- Compu_album_1
album2 <- compu_album_2
album3 <- Compu_album_3
album4 <- Compu_album_4

album1$Album <- "Harry Styles"
album2$Album <- "Fine Line"
album3$Album <- "Harry's House"
album4$Album <- "KATTDO"

combined_data <- bind_rows(album1, album2, album3, album4)

ggplot(combined_data, aes(x = Album, y = Danceability, fill = Album)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Harry Styles" = "#6a5acd",
    "Fine Line" = "#20b2aa",
    "Harry's House" = "#ff69b4",
    "KATTDO" = "#ffa500"
  )) +
  theme_minimal() +
  labs(title = "Comparison of Danceability Across Albums",
       y = "Danceability",
       x = "Album")


library(tidyverse)

album1 <- Compu_album_1
album2 <- compu_album_2
album3 <- Compu_album_3
album4 <- Compu_album_4

album1$Album <- "Harry Styles"
album2$Album <- "Fine Line"
album3$Album <- "Harry's House"
album4$Album <- "KATTDO"

combined_data <- bind_rows(album1, album2, album3, album4)

ggplot(combined_data, aes(x = Album, y = Energy, fill = Album)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "Harry Styles" = "#6a5acd",
    "Fine Line" = "#20b2aa",
    "Harry's House" = "#ff69b4",
    "KATTDO" = "#ffa500"
  )) +
  theme_minimal() +
  labs(title = "Comparison of Energy Across Albums",
       y = "Energy",
       x = "Album") +
  theme(
    plot.title = element_text(face = "bold", size = 18)
  )

# Library's nodig
library(tidyverse)
library(reshape2)

# Stel dat je combined_data al hebt zoals in je eerdere stappen
# En we gaan een SSM maken voor één album, bijv. Harry Styles
album_energy <- album1$Energy

# Maak een Self-Similarity Matrix op basis van Euclidische afstand
ssm <- outer(album_energy, album_energy, FUN = function(x, y) 1 - abs(x - y)/max(abs(album_energy - album_energy)))

# Zet het om naar dataframe voor ggplot
ssm_df <- melt(ssm)
colnames(ssm_df) <- c("Time1", "Time2", "Similarity")

# Plot de SSM
ggplot(ssm_df, aes(x = Time1, y = Time2, fill = Similarity)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Self-Similarity Matrix (Energy) - Harry Styles",
       x = "Time index",
       y = "Time index")

library(tidyverse)

# Neem album1 als voorbeeld
album_energy <- album1$Energy

# Maak een SSM (Self-Similarity Matrix) op basis van euclidische afstand
ssm <- outer(album_energy, album_energy, FUN = function(x, y) 1 - abs(x - y)/max(abs(album_energy - album_energy)))

# Zet het om naar long formaat met tidyverse
ssm_df <- as.data.frame(ssm) %>%
  mutate(Time1 = row_number()) %>%
  pivot_longer(cols = -Time1, names_to = "Time2", values_to = "Similarity") %>%
  mutate(Time2 = as.numeric(str_remove(Time2, "V")))

# Plot de SSM
ggplot(ssm_df, aes(x = Time1, y = Time2, fill = Similarity)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Self-Similarity Matrix (Energy) - Harry Styles",
       x = "Time index",
       y = "Time index")


library(tidyverse)

# Kies album1 als voorbeeld
album_energy <- album1$Energy

# Maak SSM: normaliseer eerst de energie tussen 0 en 1
norm_energy <- (album_energy - min(album_energy)) / (max(album_energy) - min(album_energy))
ssm <- outer(norm_energy, norm_energy, FUN = function(x, y) 1 - abs(x - y))

# Zet de matrix om naar long-form voor ggplot
ssm_df <- as.data.frame(ssm) %>%
  mutate(Time1 = 1:n()) %>%
  pivot_longer(cols = -Time1, names_to = "Time2", values_to = "Similarity") %>%
  mutate(Time2 = as.numeric(gsub("V", "", Time2)))  # Zorg dat Time2 numeriek is

# Plot
ggplot(ssm_df, aes(x = Time1, y = Time2, fill = Similarity)) +
  geom_tile() +
  scale_fill_gradient(low = "YELLOW", high = "GREEN") +
  theme_minimal() +
  labs(title = "Self-Similarity Matrix (Energy) - Harry Styles",
       x = "Time index",
       y = "Time index") +
  coord_fixed()  # Zorgt dat x- en y-as dezelfde schaal hebben
# Eerst installeren (één keer)
install.packages("zoo")

# Dan laden
library(zoo)

library(tidyverse)

# Kies album1 als voorbeeld
album_energy <- album1$Energy

# Genormaliseerd tussen 0 en 1
norm_energy <- (album_energy - min(album_energy)) / (max(album_energy) - min(album_energy))

# Maak een SSM met 'smoother' effecten: hier nemen we lokale gemiddelden om strepen te benadrukken
window_size <- 5  # hoe groter, hoe bredere strepen
smoothed_energy <- zoo::rollapply(norm_energy, width = window_size, mean, align = "center", fill = NA)
smoothed_energy[is.na(smoothed_energy)] <- 0  # NA's aan de randen op 0 zetten

ssm <- outer(smoothed_energy, smoothed_energy, FUN = function(x, y) 1 - abs(x - y))

# Zet om naar long-form voor ggplot
ssm_df <- as.data.frame(ssm) %>%
  mutate(Time1 = 1:n()) %>%
  pivot_longer(cols = -Time1, names_to = "Time2", values_to = "Similarity") %>%
  mutate(Time2 = as.numeric(gsub("V", "", Time2)))

# Plot
ggplot(ssm_df, aes(x = Time1, y = Time2, fill = Similarity)) +
  geom_tile() +
  scale_fill_gradient(low = "YELLOW", high = "GREEN") +
  theme_minimal() +
  labs(title = "Self-Similarity Matrix (Energy) with Streaks - Harry Styles",
       x = "Time index",
       y = "Time index") +
  coord_fixed()
theme(
  plot.title = element_text(face = "bold", size = 18)
  
  # -----------------------------
  # 1. Packages installeren (indien nodig)
  # -----------------------------
  if(!require(tuneR)) install.packages("tuneR")
  if(!require(seewave)) install.packages("seewave")
  
  sound <- Compu_album_4[["Aperture"]]
  library(tuneR)
  library(seewave)
  
  # -----------------------------
  # 2. Liedje "Aperture" uit dataset halen
  # -----------------------------
  aperture_song <- subset(Compu_album_4, title == "Aperture")
  
  # audio-object ophalen
  sound <- Aperture_song$wave[[1]]
  
  # signaal en sample rate
  x <- sound@left
  fs <- sound@samp.rate
  
  # -----------------------------
  # 3. Parameters instellen
  # -----------------------------
  window_size <- 1024
  step <- 512
  
  # aantal frames berekenen
  n_frames <- floor((length(x) - window_size) / step)
  
  # lege matrix voor cepstrogram
  cepstrogram <- matrix(0, nrow = window_size/2, ncol = n_frames)
  
  # -----------------------------
  # 4. Cepstrogram berekenen
  # -----------------------------
  for(i in 1:n_frames){
    
    start <- (i-1)*step + 1
    frame <- x[start:(start + window_size - 1)]
    
    spectrum <- fft(frame)
    
    log_spec <- log(Mod(spectrum)^2)
    
    cep <- Re(fft(log_spec, inverse = TRUE))
    
    cepstrogram[, i] <- cep[1:(window_size/2)]
  }
  
  # -----------------------------
  # 5. Cepstrogram plotten
  # -----------------------------
  image(
    t(cepstrogram),
    col = heat.colors(100),
    xlab = "Time frame",
    ylab = "Quefrency",
    main = "Cepstrogram van 'Aperture'"
  )
  
  names(Compu_album_4)
  
  barplot(
    as.numeric(Aperture[, c("Danceability","Energy","Valence","Acousticness")]),
    names.arg=c("Dance","Energy","Valence","Acoustic"),
    main="Audio features van Aperture"
  )
  
  ggplot(data_all, aes(x = valence, y = energy)) +
    geom_point() +
    geom_smooth(method = "loess") +
    facet_wrap(~album)
  
  lm(Energy ~ valence, data = Compu_album_1)
  lm(Energy ~ valence, data = compu_album_2)
  lm(Energy ~ valence, data = Compu_album_3)
  lm(Energy ~ valence, data = Compu_album_4)
  
  circshift <- function(v, n) {
    if (n == 0) v else c(tail(v, n), head(v, -n))
  }
  
  #      C     C#    D     Eb    E     F     F#    G     Ab    A     Bb    B
  major_chord <-
    c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    0,    0)
  minor_chord <-
    c(   1,    0,    0,    1,    0,    0,    0,    1,    0,    0,    0,    0)
  seventh_chord <-
    c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    1,    0)
  
  major_key <-
    c(6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
  minor_key <-
    c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)
  
  chord_templates <-
    tribble(
      ~name, ~template,
      "Gb:7", circshift(seventh_chord, 6),
      "Gb:maj", circshift(major_chord, 6),
      "Bb:min", circshift(minor_chord, 10),
      "Db:maj", circshift(major_chord, 1),
      "F:min", circshift(minor_chord, 5),
      "Ab:7", circshift(seventh_chord, 8),
      "Ab:maj", circshift(major_chord, 8),
      "C:min", circshift(minor_chord, 0),
      "Eb:7", circshift(seventh_chord, 3),
      "Eb:maj", circshift(major_chord, 3),
      "G:min", circshift(minor_chord, 7),
      "Bb:7", circshift(seventh_chord, 10),
      "Bb:maj", circshift(major_chord, 10),
      "D:min", circshift(minor_chord, 2),
      "F:7", circshift(seventh_chord, 5),
      "F:maj", circshift(major_chord, 5),
      "A:min", circshift(minor_chord, 9),
      "C:7", circshift(seventh_chord, 0),
      "C:maj", circshift(major_chord, 0),
      "E:min", circshift(minor_chord, 4),
      "G:7", circshift(seventh_chord, 7),
      "G:maj", circshift(major_chord, 7),
      "B:min", circshift(minor_chord, 11),
      "D:7", circshift(seventh_chord, 2),
      "D:maj", circshift(major_chord, 2),
      "F#:min", circshift(minor_chord, 6),
      "A:7", circshift(seventh_chord, 9),
      "A:maj", circshift(major_chord, 9),
      "C#:min", circshift(minor_chord, 1),
      "E:7", circshift(seventh_chord, 4),
      "E:maj", circshift(major_chord, 4),
      "G#:min", circshift(minor_chord, 8),
      "B:7", circshift(seventh_chord, 11),
      "B:maj", circshift(major_chord, 11),
      "D#:min", circshift(minor_chord, 3)
    )
  
  key_templates <-
    tribble(
      ~name, ~template,
      "Gb:maj", circshift(major_key, 6),
      "Bb:min", circshift(minor_key, 10),
      "Db:maj", circshift(major_key, 1),
      "F:min", circshift(minor_key, 5),
      "Ab:maj", circshift(major_key, 8),
      "C:min", circshift(minor_key, 0),
      "Eb:maj", circshift(major_key, 3),
      "G:min", circshift(minor_key, 7),
      "Bb:maj", circshift(major_key, 10),
      "D:min", circshift(minor_key, 2),
      "F:maj", circshift(major_key, 5),
      "A:min", circshift(minor_key, 9),
      "C:maj", circshift(major_key, 0),
      "E:min", circshift(minor_key, 4),
      "G:maj", circshift(major_key, 7),
      "B:min", circshift(minor_key, 11),
      "D:maj", circshift(major_key, 2),
      "F#:min", circshift(minor_key, 6),
      "A:maj", circshift(major_key, 9),
      "C#:min", circshift(minor_key, 1),
      "E:maj", circshift(major_key, 4),
      "G#:min", circshift(minor_key, 8),
      "B:maj", circshift(major_key, 11),
      "D#:min", circshift(minor_key, 3)
    )
  
  
  twenty_five |> 
    compmus_wrangle_chroma() |> 
    filter(row_number() %% 50L == 0L) |> 
    compmus_match_pitch_template(
      key_templates,         # Change to chord_templates if desired
      method = "euclidean",  # Try different distance metrics
      norm = "manhattan"     # Try different norms
    ) |>
    ggplot(
      aes(x = start + duration / 2, width = 50 * duration, y = name, fill = d)
    ) +
    geom_tile() +
    scale_fill_viridis_c(guide = "none") +
    theme_minimal() +
    labs(x = "Time (s)", y = "")
  
  