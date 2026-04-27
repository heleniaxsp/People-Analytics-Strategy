# Esta matriz proporciona una visión general de las tendencias mundiales en estrategia de capital humano.
# This matrix provides a visual overview of global trends in people strategy.
# Declaración ética: Este código fue optimizado con herramientas de IA.
# --- 1. LIBRERÍAS 
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(dplyr)) install.packages("dplyr")
library(ggplot2)
library(ggrepel)
library(dplyr)

# --- 2. DATASET 
set.seed(2026) 

data <- data.frame(
  Rol = c(
    # --- TRANSFORMACIÓN (Arriba-Derecha) ---
    # HR y Tech liderando el cambio. 
    # Coordenadas separadas.
    "HR Business Partner",    # (9.0, 8.8)
    "People Analytics Lead",  # (8.2, 9.2)
    "Agentic AI Lead",        # (9.8, 9.8)
    "Smart Contract Dev",     # (9.2, 9.4)
    "DeFi Product Owner",     # (8.6, 8.4)
    "Algorithmic Trader",     # (9.5, 9.0)
    
    # --- ESTABILIDAD (Abajo-Derecha) ---
    # El Core que protege el negocio.
    "Head of Total Rewards",  # (9.5, 4.5)
    "CISO (Cybersecurity)",   # (9.8, 5.5)
    "RegTech Architect",      # (9.2, 5.0)
    "Employee Relations",     # (8.8, 3.5)
    "Core Banking Dev",       # (9.0, 4.0)
    "Data Privacy Officer",   # (8.5, 3.0)
    
    # --- FLEXIBILIDAD (Arriba-Izquierda) ---
    # Gig Economy / Contratos ágiles.
    "Tech Recruiter (Gig)",   # (5.0, 8.5)
    "Sourcing Specialist",    # (4.5, 7.5)
    "Web3 Community Mgr",     # (3.5, 9.0)
    "QA Tester (Contract)",   # (5.5, 8.0)
    "UX Writer (Project)",    # (6.0, 7.2)
    
    # --- EFICIENCIA (Abajo-Izquierda) ---
    # Automatización pura.
    "HR Coordinator",         # (3.5, 3.5)
    "Payroll Specialist",     # (5.0, 2.5)
    "IT Support L1",          # (4.0, 3.0)
    "Onboarding Coord.",      # (2.5, 2.0)
    "KYC Analyst L1"          # (2.0, 1.5)
  ),
  
  # Categorías para formas (Shapes)
  Area = c(
    rep("HR Strategy", 2), rep("Tech & AI", 2), rep("Fintech", 2), # Transformación
    rep("HR Core", 2), rep("Risk & Tech", 4),                      # Estabilidad
    rep("Talent Acq", 2), rep("Marketing/Prod", 3),                # Flexibilidad
    rep("HR Ops", 2), rep("Soporte Tech", 3)                       # Eficiencia
  ),
  
  # CRITICIDAD (0-10): Impacto Estratégico
  Criticidad = c(
    9.0, 8.2, 9.8, 9.2, 8.6, 9.5,   # Transformación
    9.5, 9.8, 9.2, 8.8, 9.0, 8.5,   # Estabilidad
    5.0, 4.5, 3.5, 5.5, 6.0,        # Flexibilidad
    3.5, 5.0, 4.0, 2.5, 2.0         # Eficiencia
  ),
  
  # DINAMISMO (0-10): Velocidad de Cambio
  Dinamismo = c(
    8.8, 9.2, 9.8, 9.4, 8.4, 9.0,   # Transformación
    4.5, 5.5, 5.0, 3.5, 4.0, 3.0,   # Estabilidad
    8.5, 7.5, 9.0, 8.0, 7.2,        # Flexibilidad
    3.5, 2.5, 3.0, 2.0, 1.5         # Eficiencia
  )
)

# Definir Nombres de Zona 
data <- data %>%
  mutate(Zona = case_when(
    Criticidad >= 7 & Dinamismo >= 7 ~ "TRANSFORMACIÓN\n(Invertir & Upskill)",
    Criticidad >= 7 & Dinamismo < 7  ~ "ESTABILIDAD\n(Retener / Fidelizar)",
    Criticidad < 7 & Dinamismo >= 7  ~ "FLEXIBILIDAD\n(Gig Economy / Borrow)",
    TRUE                             ~ "EFICIENCIA\n(Automatizar con IA)"
  ))

# --- 3. COLORES
cols <- c(
  "TRANSFORMACIÓN\n(Invertir & Upskill)" = "#C0392B",  # Rojo Oscuro (High Stakes)
  "ESTABILIDAD\n(Retener & Fidelizar)"   = "#2E86C1",  # Azul (Trust)
  "FLEXIBILIDAD\n(Gig Economy / Borrow)" = "#F39C12",  # Amarillo (Agile)
  "EFICIENCIA\n(Automatizar con IA)"     = "#7F8C8D"   # Gris (Neutral)
)

# --- 4. GRÁFICO OPTIMIZADO 
p <- ggplot(data, aes(x = Criticidad, y = Dinamismo)) +
  
  # Líneas Guía (Ejes centrales)
  geom_vline(xintercept = 7, linetype = "dashed", color = "gray75", size = 0.6) +
  geom_hline(yintercept = 7, linetype = "dashed", color = "gray75", size = 0.6) +
  
  # Puntos (Grandes y claros)
  geom_point(aes(color = Zona, shape = Area), size = 6, stroke = 1.3) +
  
  # Formas variadas para identificar áreas
  scale_shape_manual(values = c(15, 17, 18, 16, 8, 3, 4, 10, 12, 14)) + 
  scale_color_manual(values = cols) +
  
  # --- ETIQUETAS DE ZONA (EXTERNAS Y SIN NÚMEROS) 
  # Transformación (Arriba Derecha)
  annotate("text", x = 12, y = 12, label = "TRANSFORMACIÓN", 
           hjust = 1, fontface = "bold", color = "#C0392B", size = 4) +
  
  # Estabilidad (Abajo Derecha)
  annotate("text", x = 12, y = 0.2, label = "ESTABILIDAD (CORE)", 
           hjust = 1, vjust = 0, fontface = "bold", color = "#2E86C1", size = 4) +
  
  # Flexibilidad (Arriba Izquierda)
  annotate("text", x = 0.2, y = 12, label = "FLEXIBILIDAD (GIG)", 
           hjust = 0, fontface = "bold", color = "#F39C12", size = 4) +
  
  # Eficiencia (Abajo Izquierda)
  annotate("text", x = 0.2, y = 0.2, label = "EFICIENCIA (AUTO)", 
           hjust = 0, vjust = 0, fontface = "bold", color = "#7F8C8D", size = 4) +
  
  # --- ETIQUETAS DE ROLES (CONTROL TOTAL DE ESPACIO) 
  geom_text_repel(aes(label = Rol), 
                  size = 3.6,           # Tamaño legible
                  fontface = "bold", 
                  color = "#2C3E50",
                  # PARÁMETROS CLAVE ANTI-SOLAPAMIENTO:
                  box.padding = 0.8,    # Mucho espacio alrededor del texto
                  point.padding = 0.5,  # Distancia del punto
                  force = 80,           # Fuerza de repulsión extrema
                  max.overlaps = Inf,   # Nunca ocultar etiquetas
                  min.segment.length = 0, 
                  segment.size = 0.3,
                  segment.color = "#BDC3C7",
                  bg.color = "white",   # Halo blanco para leer sobre líneas
                  bg.r = 0.15) +
  
  # --- ESCALAS AMPLIADAS (0-12) 
  # Esto crea el "lienzo" extra para que las etiquetas floten sin cortarse
  scale_x_continuous(breaks = seq(0, 10, 2), limits = c(0, 12.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 12.5), expand = c(0, 0)) +
  
  # --- TÍTULOS 
  labs(
    title = "MATRIZ ESTRATÉGICA DE TALENTO 2026",
    subtitle = "Priorización de roles HR, Tech & Fintech basada en Impacto y Dinamismo",
    x = "ÍNDICE DE CRITICIDAD (Impacto en P&L) →",
    y = "ÍNDICE DE DINAMISMO (Velocidad de Cambio) →",
    caption = "Fuentes: Gartner Trends 2026, WEF & ADP | Elaboración : Helen Lopez Calderon",
    shape = "ÁREA FUNCIONAL",
    color = "ESTRATEGIA RECOMENDADA"
  ) +
  
  theme_classic() + 
  theme(
    # Márgenes 
    plot.margin = margin(50, 50, 40, 40),
    
    # Títulos
    plot.title = element_text(face = "bold", size = 18, color = "#17202A"),
    plot.subtitle = element_text(size = 12, color = "#566573", margin = margin(b=20)),
    
    # Ejes
    axis.title = element_text(face = "bold", size = 10, color = "#2C3E50"),
    axis.line = element_line(arrow = arrow(length = unit(0.2, "cm"), type = "closed")),
    
    # Leyenda
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 10),
    legend.margin = margin(t=20)
  ) +
  
  # Permitir etiquetas fuera del gráfico
  coord_cartesian(clip = "off", xlim = c(0, 12), ylim = c(0, 12))

# Guardar
ggsave("Matriz_Talento_2026.png", plot = p, width = 12, height = 12, dpi = 300)
print(p)
