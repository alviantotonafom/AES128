library(shiny)
library(openssl)

# Fungsi untuk enkripsi file dengan AES 128
encrypt_file <- function(file_path, key) {
  file_data <- readBin(file_path, what = "raw", n = file.info(file_path)$size)
  
  # Buat kunci dan IV
  key <- charToRaw(key)
  iv <- rand_bytes(16)  # Inisialisasi IV acak dengan panjang 16 byte
  
  # Enkripsi menggunakan AES 128
  encrypted_data <- aes_cbc_encrypt(file_data, key, iv)  # Enkripsi dengan IV
  
  # Simpan IV dan data terenkripsi ke dalam file
  encrypted_file_path <- paste0(file_path, ".enc")
  writeBin(c(iv, encrypted_data), encrypted_file_path)  # Simpan IV dan data terenkripsi
  
  return(encrypted_file_path)
}

# Fungsi untuk dekripsi file dengan AES 128
decrypt_file <- function(file_path, key) {
  # Membaca file terenkripsi
  file_data <- readBin(file_path, what = "raw", n = file.info(file_path)$size)
  
  # Ekstrak IV dari data
  iv <- file_data[1:16]  # Ambil 16 byte pertama sebagai IV
  encrypted_data <- file_data[-(1:16)]  # Ambil sisa data sebagai data terenkripsi
  
  # Dekripsi menggunakan AES 128
  key <- charToRaw(key)
  decrypted_data <- aes_cbc_decrypt(encrypted_data, key, iv)  # Dekripsi dengan IV
  
  # Simpan data terdekripsi dengan nama file asli (tanpa .enc)
  decrypted_file_path <- sub("\\.enc$", "", file_path)  # Menghapus ekstensi .enc
  writeBin(decrypted_data, decrypted_file_path)  # Simpan data terdekripsi
  
  return(decrypted_file_path)
}

# Antarmuka pengguna Shiny
ui <- fluidPage(
  titlePanel("Aplikasi Enkripsi dan Dekripsi File"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Pilih file (PDF, Word, Excel, PNG, JPG, NFF)", 
                accept = c(".pdf", ".docx", ".xlsx", ".png", ".jpg", ".nff")),
      textInput("key", "Masukkan Kunci Enkripsi (16 karakter)", value = "KELOMPOK12345678"),
      actionButton("encrypt", "Enkripsi"),
      actionButton("decrypt", "Dekripsi"),
      textOutput("status")
    ),
    mainPanel(
      tableOutput("fileinfo"),
      downloadButton("downloadEncrypted", "Download File Terenkripsi"),
      downloadButton("downloadDecrypted", "Download File Terdekripsi")
    )
  )
)

# Server logika
server <- function(input, output, session) {
  file_info <- reactive({
    req(input$file)
    input$file
  })
  
  output$fileinfo <- renderTable({
    if (is.null(file_info())) return(NULL)
    file_info()
  })
  
  encrypted_file_path <- reactiveVal(NULL)
  decrypted_file_path <- reactiveVal(NULL)
  
  observeEvent(input$encrypt, {
    req(input$file)
    
    # Validasi panjang kunci enkripsi
    if (nchar(input$key) != 16) {
      output$status <- renderText("Kunci harus 16 karakter.")
      return(NULL)
    }
    
    # Validasi ukuran file maksimal 1MB
    if (input$file$size > 1e6) {
      output$status <- renderText("Ukuran file maksimal 1MB.")
      return(NULL)
    }
    
    tryCatch({
      encrypted_file <- encrypt_file(input$file$datapath, input$key)
      encrypted_file_path(encrypted_file)
      output$status <- renderText("File berhasil dienkripsi.")
    }, error = function(e) {
      output$status <- renderText(paste("Error saat enkripsi:", e$message))
    })
  })
  
  observeEvent(input$decrypt, {
    req(encrypted_file_path())
    
    tryCatch({
      decrypted_file <- decrypt_file(encrypted_file_path(), input$key)
      decrypted_file_path(decrypted_file)
      output$status <- renderText("File berhasil didekripsi.")
    }, error = function(e) {
      output$status <- renderText(paste("Error saat dekripsi:", e$message))
    })
  })
  
  output$downloadEncrypted <- downloadHandler(
    filename = function() {
      paste0(file_info()$name, ".enc")
    },
    content = function(file) {
      file.copy(encrypted_file_path(), file)
    }
  )
  
  output$downloadDecrypted <- downloadHandler(
    filename = function() {
      # Kembalikan nama file terdekripsi sama dengan nama asli
      paste0(sub("\\.enc$", "", file_info()$name))
    },
    content = function(file) {
      file.copy(decrypted_file_path(), file)
    }
  )
}

# Jalankan aplikasi
shinyApp(ui, server)
