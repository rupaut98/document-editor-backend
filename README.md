# Document Editor

A Haskell-based web application for creating, editing, and managing documents with user authentication and real-time collaboration features.

## Table of Contents

- [Features](#features)
- [Technologies Used](#technologies-used)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Configuration](#configuration)
- [Database Setup](#database-setup)
- [Running the Application](#running-the-application)
- [API Endpoints](#api-endpoints)
- [Contributing](#contributing)
- [License](#license)

## Features

- **User Authentication:** Secure user registration and login using JWT tokens.
- **Document Management:** Create, read, update, and delete documents.
- **Real-Time Collaboration:** Collaborative editing of documents via WebSockets.
- **Environment Configuration:** Securely manage environment variables using `.env` files.
- **API Documentation:** Well-defined API endpoints using Servant.

## Technologies Used

- **Haskell:** Functional programming language for robust and type-safe code.
- **Servant:** Type-safe web framework for defining and serving APIs.
- **Beam:** Database library for interacting with PostgreSQL.
- **PostgreSQL:** Relational database for storing user and document data.
- **JWT (JSON Web Tokens):** For secure authentication.
- **dotenv:** Manage environment variables.
- **WebSockets:** Enable real-time collaboration features.

## Prerequisites

Ensure you have the following installed on your system:

- **GHC (Glasgow Haskell Compiler):** Version 9.4.8 or compatible.
- **Cabal:** The Haskell package manager.
- **PostgreSQL:** Version 12 or higher.
- **Git:** For version control.
- **Make:** For building the project (optional but recommended).

## Installation

1. **Clone the Repository:**

   ```bash
   git clone https://github.com/yourusername/document-editor.git
   cd document-editor
   ```

2. **Install Dependencies:**
    Ensure cabal is up-to-date and install necessary dependencies.

    ```bash
    cabal update
    cabal install --only-dependencies
    ```

3. **Configuration:**

**Environment Variables:**
Create a .env file in the root directory to store your environment variables securely.
```bash
touch .env
```

Important: Add .env to your .gitignore to prevent sensitive information from being committed.
```bash
echo ".env" >> .gitignore
```

4. **Database Configuration:**

The application uses PostgreSQL for data storage. Ensure the credentials in your .env file match your PostgreSQL setup.

**Create the Database:**
Log into PostgreSQL and create the required database.
```bash
psql -U postgres
```

Within the PostgreSQL shell:
```sql
CREATE DATABASE documents_db;
CREATE USER rupakraut WITH PASSWORD 'rupakraut';
GRANT ALL PRIVILEGES ON DATABASE documents_db TO rupakraut;
\q
```

**Run Migrations (If Applicable):**
If your project uses Beam's migration tools, run the necessary migrations to set up the database schema.
```bash
cabal run
```

(Ensure your Main.hs handles migrations on startup.)

**Build the Project:**
```bash
cabal build
```

**Run the Application:**
```bash
cabal run
```

The server should start and listen on http://localhost:8080.