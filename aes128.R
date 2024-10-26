library(shiny)
library(openssl)

# Fungsi untuk enkripsi file dengan AES 128
encrypt_file <- function(file_path, key) {
  file_data <- readBin(file_path, what = "raw", n = file.info(file_path)$size)
  
  # Buat kunci dan IV
  key <- charToRaw(key)
  iv <- rand_bytes(16)
  
  # Enkripsi menggunakan AES 128
  encrypted_data <- aes_cbc_encrypt(file_data, key, iv)
  
  # Simpan IV dan data terenkripsi ke dalam file
  encrypted_file_path <- paste0(file_path, ".enc")
  writeBin(c(iv, encrypted_data), encrypted_file_path)
  
  return(encrypted_file_path)
}

# Fungsi untuk dekripsi file dengan AES 128
decrypt_file <- function(file_path, key) {
  file_data <- readBin(file_path, what = "raw", n = file.info(file_path)$size)
  
  # Ekstrak IV dari data
  iv <- file_data[1:16]
  encrypted_data <- file_data[-(1:16)]
  
  # Dekripsi menggunakan AES 128
  key <- charToRaw(key)
  
  decrypted_data <- aes_cbc_decrypt(encrypted_data, key, iv)
  
  decrypted_file_path <- sub("\\.enc$", "", file_path)
  writeBin(decrypted_data, decrypted_file_path)
  
  return(decrypted_file_path)
}

# Antarmuka pengguna Shiny
ui <- fluidPage(
  titlePanel("AES128 File Encryption & Decryption"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Enkripsi",
                 h3("Panduan Enkripsi:"),
                 tags$ul(
                   tags$li("Pilih file yang ingin dienkripsi (mendukung PDF, Word, Excel, PNG, JPG, TIFF)."),
                   tags$li("Masukkan kunci enkripsi sepanjang 16 karakter."),
                   tags$li("Tekan tombol 'Enkripsi' untuk memulai proses enkripsi."),
                   tags$li("Setelah selesai, unduh file terenkripsi.")
                 ),
                 fileInput("encrypt_file", "Pilih file", 
                           accept = c(".pdf", ".docx", ".xlsx", ".png", ".jpg", ".tiff")),
                 textInput("encrypt_key", "Masukkan Kunci Enkripsi (16 karakter)", value = "KUNCI-KELOMPOK-1"),
                 actionButton("encrypt_button", "Enkripsi"),
                 textOutput("encrypt_status"),
                 textOutput("encrypt_time"),
                 downloadButton("downloadEncrypted", "Download File Terenkripsi")
        ),
        tabPanel("Dekripsi",
                 h3("Panduan Dekripsi:"),
                 tags$ul(
                   tags$li("Pilih file yang sudah dienkripsi sebelumnya (dengan ekstensi '.enc')."),
                   tags$li("Masukkan kunci dekripsi yang sama dengan kunci enkripsi."),
                   tags$li("Tekan tombol 'Dekripsi' untuk memulai proses dekripsi."),
                   tags$li("Setelah selesai, unduh file terdekripsi.")
                 ),
                 fileInput("decrypt_file", "Pilih file terenkripsi (.enc)", accept = ".enc"),
                 textInput("decrypt_key", "Masukkan Kunci Dekripsi (16 karakter)", value = "KUNCI-KELOMPOK-1"),
                 actionButton("decrypt_button", "Dekripsi"),
                 textOutput("decrypt_status"),
                 textOutput("decrypt_time"),
                 downloadButton("downloadDecrypted", "Download File Terdekripsi")
        )
      )
    ),
    mainPanel(
      tableOutput("file_info")
    )
  )
)

# Server logika
server <- function(input, output, session) {
  encrypted_file_path <- reactiveVal(NULL)
  decrypted_file_path <- reactiveVal(NULL)
  
  # Proses Enkripsi
  observeEvent(input$encrypt_button, {
    req(input$encrypt_file)
    
    if (nchar(input$encrypt_key) != 16) {
      output$encrypt_status <- renderText("Kunci harus 16 karakter.")
      return(NULL)
    }
    
    if (input$encrypt_file$size > 1e6) {
      output$encrypt_status <- renderText("Ukuran file maksimal 1MB.")
      return(NULL)
    }
    
    start_time <- Sys.time()
    tryCatch({
      encrypted_file <- encrypt_file(input$encrypt_file$datapath, input$encrypt_key)
      encrypted_file_path(encrypted_file)
      runtime <- Sys.time() - start_time
      output$encrypt_time <- renderText(paste("Waktu enkripsi:", round(runtime, 2), "detik"))
      output$encrypt_status <- renderText("File berhasil dienkripsi.")
    }, error = function(e) {
      output$encrypt_status <- renderText(paste("Error saat enkripsi:", e$message))
    })
  })
  
  # Proses Dekripsi
  observeEvent(input$decrypt_button, {
    req(input$decrypt_file)
    
    if (nchar(input$decrypt_key) != 16) {
      output$decrypt_status <- renderText("Kunci harus 16 karakter.")
      return(NULL)
    }
    
    start_time <- Sys.time()
    tryCatch({
      decrypted_file <- decrypt_file(input$decrypt_file$datapath, input$decrypt_key)
      decrypted_file_path(decrypted_file)
      runtime <- Sys.time() - start_time
      output$decrypt_time <- renderText(paste("Waktu dekripsi:", round(runtime, 2), "detik"))
      output$decrypt_status <- renderText("File berhasil didekripsi.")
    }, error = function(e) {
      output$decrypt_status <- renderText("Error saat dekripsi: Kunci tidak sesuai atau file tidak valid.")
    })
  })
  
  # Download file terenkripsi dan refresh setelah download
  output$downloadEncrypted <- downloadHandler(
    filename = function() {
      paste0(input$encrypt_file$name, ".enc")
    },
    content = function(file) {
      file.copy(encrypted_file_path(), file)
      encrypted_file_path(NULL)
      session$reload()
    }
  )
  
  # Download file terdekripsi dan refresh setelah download
  output$downloadDecrypted <- downloadHandler(
    filename = function() {
      sub("\\.enc$", "", input$decrypt_file$name)
    },
    content = function(file) {
      file.copy(decrypted_file_path(), file)
      decrypted_file_path(NULL)
      session$reload()
    }
  )
  
  # Tampilkan info file
  output$file_info <- renderTable({
    if (is.null(input$encrypt_file) && is.null(input$decrypt_file)) return(NULL)
    if (!is.null(input$encrypt_file)) {
      return(input$encrypt_file)
    } else {
      return(input$decrypt_file)
    }
  })
}

# Jalankan aplikasi
shinyApp(ui, server)
