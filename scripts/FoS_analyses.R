# ============================================================
# Kara??ukur Landslide ??? FoS Analysis
# Factor of Safety (FoS) sensitivity analysis for planar/wedge
# failure under varying shear strength, pore pressure, and
# toe-support conditions (Monzogranite slope)
# ============================================================

# --- Constants ---
H_vals <- c(30, 40)         # Effective failure height (m)
beta_eff <- 67.7            # Intersection plunge of Set-2 & Set-3 (deg)
gamma_use <- 25.80          # Saturated unit weight (kN/m3) [monzogranite]

phi_grid <- seq(20, 45, by = 1)      # Internal friction angle range (deg)
c_grid   <- seq(0, 150, by = 5)      # Cohesion range (kPa) ??? extended beyond 0???50
ru_grid  <- c(0.0, 0.1, 0.2, 0.3, 0.4) # Pore pressure ratio (ru)
toe_grid <- c(1.0, 1.1, 1.2)         # Toe-support / excavation factor

# Degree-to-radian conversion
deg2rad <- function(x) x * pi / 180

# --- FoS Function (Limit Equilibrium, Planar Sliding with ru) ---
fos_planar_ru <- function(cj, phij, gamma, H, beta, ru = 0, toe_factor = 1) {
  b <- deg2rad(beta)
  phi <- deg2rad(phij)
  
  # Driving block weight (unit width)
  W <- 0.5 * gamma * H^2 / tan(b)  # kN/m
  L <- H / sin(b)                  # Sliding surface length (m)
  
  # Normal and shear components
  N <- W * cos(b)
  T <- W * sin(b)
  
  # Effective normal force and amplified driving force
  N_eff <- N * (1 - ru)
  T_eff <- T * toe_factor
  
  # Resisting and driving forces
  R <- (cj * L) + N_eff * tan(phi)
  FoS <- R / T_eff
  
  return(FoS)
}

# --- Parameter Space Exploration ---
out <- list()
k <- 1

for (H in H_vals) {
  for (ru in ru_grid) {
    for (toeF in toe_grid) {
      for (phij in phi_grid) {
        for (cj in c_grid) {
          out[[k]] <- data.frame(
            H = H,
            ru = ru,
            toeF = toeF,
            phij = phij,
            cj = cj,
            FoS = fos_planar_ru(cj, phij, gamma_use, H, beta_eff, ru, toeF)
          )
          k <- k + 1
        }
      }
    }
  }
}

# Combine results into a single data frame
df <- do.call(rbind, out)

# Identify critical cases (FoS < 1)
df$critical <- df$FoS < 1

# --- Summary: percentage of critical cases ---
aggregate(critical ~ H + ru + toeF, df, function(x) mean(x) * 100)

# --- Combinations closest to FoS = 1 (limit equilibrium threshold) ---
df$dist1 <- abs(df$FoS - 1)
head(df[order(df$dist1), c("H", "ru", "toeF", "phij", "cj", "FoS")], 30)
