# 假设 X 的 rownames 包含每本书的名称
# 获取行名（书名）
book_names <- rownames(X)

# 为每本书指定作者对应的颜色映射
# 定义作者名称或簇名称与行名的对应关系 (需要根据你的实际情况调整)
author_mapping <- list(
  "Stoker" = c("Stoker_Dracula.txt", "Stoker_Man.txt", "Stoker_Sea.txt"),
  "Brown" = c("Brown_Arthur.txt", "Brown_Edgar.txt", "Brown_Jane.txt", "Brown_Ormond.txt", "Brown_Wieland.txt"),
  "Shelley" = c("Shelleys_Tour.txt"),
  "MShelley" = c("MShelley_Falkner.txt", "MShelley_LastMan.txt", "MShelley_Lodore.txt", "MShelley_Mathilda.txt", "MShelley_Valperga.txt", "MShelley_Warbeck.txt"),
  "Wollstonecraft" = c("Wollstonecraft_Maria.txt", "Wollstonecraft_Mary.txt"),
  "PShelley" = c("PShelley_StIrvyne.txt", "PShelley_Zastrozzi.txt"),
  "PShelleyPoet" = c("PShelleyPoet_1.txt", "PShelleyPoet_2.txt", "PShelleyPoet_3.txt"),
  "Peacock" = c("Peacock_Crotchet.txt", "Peacock_Headlong.txt", "Peacock_Marian.txt", "Peacock_Nightmare.txt"),
  "Frankenstein" = c("Classify_Frankenstein.txt"),
  "Scott" = c("Scott_Durward.txt", "Scott_Guy.txt", "Scott_Ivanhoe.txt", "Scott_Kenilworth.txt", "Scott_RobRoy.txt", "Scott_Talisman.txt"),
  "Godwin" = c("Godwin_Caleb.txt", "Godwin_Damon.txt", "Godwin_Fleetwood.txt", "Godwin_Imogen.txt", "Godwin_StLeon.txt"),
  "Polidori" = c("Polidori_Vampyre.txt")
)

# 定义 12 个颜色，分别对应 12 个作者
author_colors <- c("red", "blue", "green", "purple", "orange", "yellow",
                   "brown", "pink", "grey", "cyan", "magenta", "black")

# 创建一个空的颜色向量，用于存放每本书的颜色
color_vector <- rep(NA, length(book_names))

# 根据书名（行名）进行颜色映射
for (i in seq_along(book_names)) {
  for (j in seq_along(author_mapping)) {
    if (book_names[i] %in% author_mapping[[j]]) {
      color_vector[i] <- author_colors[j]
    }
  }
}