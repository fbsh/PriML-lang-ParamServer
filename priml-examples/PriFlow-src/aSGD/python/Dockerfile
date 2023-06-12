# Use the official Python image from the Docker Hub
FROM python:3.10

# Set the working directory to /app
WORKDIR /app

# Copy the contents of the local src directory to the working directory
COPY . .

# Use pip to install required dependencies
RUN pip install numpy scipy scikit-learn joblib autograd

# Specify the command to run on container start
CMD ["python", "./test.py"]
