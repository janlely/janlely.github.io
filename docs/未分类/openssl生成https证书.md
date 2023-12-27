## 生成CA证书
ca-gen.sh
```bash
#!/bin/bash

# Set the desired CN for your CA
CACN="example.com"

# Step 1: Generate the CA private key
openssl genpkey -algorithm RSA -out example-ca.key -aes256

# Step 2: Generate the self-signed CA certificate
openssl req -x509 -new -nodes -key example-ca.key -sha256 -days 1825 -out example-ca.crt -subj "/C=CN/ST=HB/L=XA/O=EXAMPLE/OU=EXAMPLE/CN=${CACN}"

# Verify the CA certificate
openssl x509 -in example-ca.crt -text -noout
```

## 创建域名证书的配置文件
example.cnf
```bash
[req]
default_bits = 2048
prompt = no
default_md = sha256
distinguished_name = dn
req_extensions = req_ext

[dn]
C=CN
ST=HB
L=XA
O=EXAMPLE #Org
OU=EXAMPLE #Company
CN=*.example.com #Domain

[req_ext]
subjectAltName = @alt_names

[alt_names]
DNS.1 = *.example.com #Domain
DNS.2 = example.com #Domain
```

## 生成域名证书并签名
./cert-gen.sh example.cnf
```bash
#!/bin/bash

# Step 1: Generate the private key for the wildcard certificate
openssl genpkey -algorithm RSA -out wildcard.key

# Step 2: Create the CSR using the configuration and the private key
openssl req -new -key wildcard.key -out wildcard.csr -config $1 

# Step 3: Sign the CSR with your CA certificate and private key
openssl x509 -req -in wildcard.csr -CA example-ca.crt -CAkey example-ca.key -CAcreateserial -out wildcard.crt -days 365 -sha256 -extfile $1 -extensions req_ext

# Validate the created wildcard certificate
openssl x509 -in wildcard.crt -text -noout
```