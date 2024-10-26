# Aplikasi Enkripsi dan Dekripsi File

## Deskripsi
Aplikasi ini memungkinkan pengguna untuk mengenkripsi dan mendekripsi file menggunakan algoritma AES 128 bit. Dengan aplikasi ini, pengguna dapat menjaga keamanan data mereka dengan mudah.

## Fitur
- Enkripsi file (PDF, DOCX, XLSX, PNG, JPG, NFF) dengan kunci 16 karakter.
- Dekripsi file terenkripsi dengan kunci yang sama.
- Antarmuka pengguna yang sederhana dan intuitif.
- Menampilkan informasi file yang diunggah.

## Prerequisites
Sebelum menjalankan aplikasi ini, pastikan Anda telah menginstal:
- R (versi [masukkan versi R yang dibutuhkan])
- Paket R yang diperlukan: `shiny` dan `openssl`. (Instal dengan menjalankan: `install.packages(c("shiny", "openssl"))`)

## Cara Menjalankan Aplikasi
1. **Clone Repository** (atau unduh file):
   ```bash
   git clone [URL_REPOSITORY]
   ```
2. **Buka R**: Buka terminal R atau RStudio.
3. **Setel Direktori Kerja**: Atur direktori kerja ke lokasi di mana file aes128.R berada:
    ```bash
    setwd("path/to/your/directory")
    ```
4. **Jalankan Aplikasi**: Load paket shiny dan jalankan aplikasi dengan:
    ```bash
    library(shiny)
    runApp("aes128.R")
    ```

## Penggunaan

### Enkripsi
1. **Pilih file** yang ingin dienkripsi.
2. **Masukkan kunci enkripsi** (harus 16 karakter).
3. **Klik tombol** **Enkripsi File** untuk mengenkripsi file.
4. **Unduh file terenkripsi** menggunakan tombol **Download File Terenkripsi**.

### Dekripsi
1. **Pilih file terenkripsi** (dengan ekstensi `.enc`).
2. **Masukkan kunci dekripsi** (harus sama dengan kunci enkripsi).
3. **Klik tombol** **Dekripsi File** untuk mendekripsi file.
4. **Unduh file terdekripsi** menggunakan tombol **Download File Terdekripsi**.
