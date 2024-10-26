# Aplikasi Enkripsi dan Dekripsi File

## Deskripsi
Aplikasi ini memungkinkan pengguna untuk mengenkripsi dan mendekripsi file secara aman menggunakan algoritma AES 128-bit. Aplikasi ini dirancang dengan antarmuka sederhana yang memudahkan pengguna untuk menjaga keamanan data mereka.

## Fitur
- **Dukungan Format File**: Mendukung file PDF, DOCX, XLSX, PNG, JPG, dan TIFF.
- **Enkripsi dan Dekripsi**: Melakukan enkripsi dengan kunci 16 karakter dan dekripsi menggunakan kunci yang sama.
- **Antarmuka Ramah Pengguna**: Desain sederhana, dengan panduan penggunaan yang jelas di aplikasi.
- **Penanganan File Aman**: Batas ukuran file hingga 1 MB untuk proses optimal.

## Prasyarat
Pastikan sistem Anda telah menginstal:
- **R** (versi minimum disarankan: 4.0)
- Paket R yang dibutuhkan:
  ```R
  install.packages(c("shiny", "openssl"))

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
1. Pilih file yang ingin dienkripsi dari perangkat.
2. Masukkan kunci enkripsi (harus 16 karakter).
3. Klik Enkripsi File untuk memulai proses enkripsi.
4. Setelah proses selesai, tombol Download File Terenkripsi akan muncul. Klik untuk mengunduh file yang telah terenkripsi.
5. Catatan: Proses ini akan otomatis menyegarkan aplikasi setelah file berhasil diunduh.

### Dekripsi
1. Pilih file terenkripsi yang ingin didekripsi (dengan ekstensi .enc).
2. Masukkan kunci yang sama seperti saat enkripsi (16 karakter).
3. Klik Dekripsi File untuk memulai proses dekripsi.
4. Setelah selesai, klik Download File Terdekripsi untuk mengunduh file hasil dekripsi.
5. Catatan: Sama seperti enkripsi, aplikasi akan menyegarkan setelah unduhan berhasil.
