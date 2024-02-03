import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.metrics import classification_report, confusion_matrix
import matplotlib.pyplot as plt
import seaborn as sns


data = pd.read_csv('Win_Lose_Data.csv')
data 

data.head()

data.shape

data.describe()

# Convert 'status' column to binary (win = 1, lose and draw = 0)
data['status'] = data['status'].apply(lambda x: 1 if x == 'win' else 0)

# Select the zodiac sign columns (F to U) and the win rate column (E)
zodiac_columns = ['Aries', 'Leo', 'Sagittarius', 'Cancer', 'Scopio', 'Pisces', 'Taurus', 'Virgo', 'Capricorn', 'Gemini', 'Libra', 'Aquarius']
target_column = 'status'

zodiac_columns

# Splitting the dataset into features (X) and target (Y)
X = data[zodiac_columns]
Y = data[target_column]

print("Class distribution in the dataset:\n", data[target_column].value_counts())

# Splitting the data into training and testing sets
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.2, random_state=42)

# Training the model using Gradient Boosting Classifier
gb_model = GradientBoostingClassifier(random_state=42)
gb_model.fit(X_train, Y_train)

# Predicting on the test set
Y_pred_gb = gb_model.predict(X_test)

# Model Evaluation
print("Classification Report:\n", classification_report(Y_test, Y_pred_gb))
conf_mat = confusion_matrix(Y_test, Y_pred_gb)

report = classification_report(Y_test, Y_pred_gb, output_dict=True)
# Extracting specific performance metrics
category_0_precision = report['0']['precision']
category_0_recall = report['0']['recall']
category_0_f1 = report['0']['f1-score']

category_1_precision = report['1']['precision']
category_1_recall = report['1']['recall']
category_1_f1 = report['1']['f1-score']

overall_accuracy = report['accuracy']

print(f"Category 0 (Loss) - Precision: {category_0_precision:.2f}, Recall: {category_0_recall:.2f}, F1 Score: {category_0_f1:.2f}")
print(f"Category 1 (Win) - Precision: {category_1_precision:.2f}, Recall: {category_1_recall:.2f}, F1 Score: {category_1_f1:.2f}")
print(f"Overall Accuracy: {overall_accuracy:.2f}")

# Confusion Matrix Visualization
plt.figure(figsize=(8, 6))
sns.heatmap(conf_mat, annot=True, fmt='g', cmap='Blues')
plt.title('Confusion Matrix')
plt.xlabel('Predicted Label')
plt.ylabel('True Label')
plt.show()

# Define the input data: counts of each zodiac sign in a team
input_data = (1, 1, 1, 1, 0, 0, 0, 1, 1, 3, 1, 1)

# Convert the input data to a NumPy array
input_data_as_numpy_array = np.asarray(input_data)

# Reshape the data to fit the model's expectation for a single sample prediction
input_data_reshaped = input_data_as_numpy_array.reshape(1, -1)

# Use the trained model to make a prediction
prediction = gb_model.predict(input_data_reshaped)
prediction_proba = gb_model.predict_proba(input_data_reshaped)

print("Prediction:", prediction)
print("Probability of each class:", prediction_proba)

# Assuming prediction is the output from your model
if prediction[0] == 1:
    print("The prediction indicates a win.")
else:
    print("The prediction indicates a loss.")
    
    
    
from sklearn.metrics import roc_curve, auc
import matplotlib.pyplot as plt

# Predict probabilities for the positive class
Y_pred_proba = gb_model.predict_proba(X_test)[:, 1]

# Compute ROC curve and AUC
fpr, tpr, thresholds = roc_curve(Y_test, Y_pred_proba)
roc_auc = auc(fpr, tpr)


# Plotting
plt.figure(figsize=(8, 6))
plt.plot(fpr, tpr, color='black', lw=2)  # Change the line color to black
plt.fill_between(fpr, tpr, step='post', alpha=0.2, color='grey')  # Fill area under the curve
plt.plot([0, 1], [0, 1], color='darkgray', lw=2, linestyle='--')
plt.xlabel('1 - Specificity')
plt.ylabel('Sensitivity')
plt.title('ROC Curve')

# Display AUC on the plot
plt.text(0.05, 0.95, f'AUC = {roc_auc:.2f}', fontsize=12, va='bottom', ha='left')

plt.show()


# Extracting feature importances
feature_importances = gb_model.feature_importances_

# Creating a DataFrame for easier visualization
features_df = pd.DataFrame({'Feature': X_train.columns, 'Importance': feature_importances})
features_df = features_df.sort_values(by='Importance', ascending=False)

# Displaying the most important features
print(features_df)


import matplotlib.pyplot as plt
import seaborn as sns

# Setting the style for the plot
sns.set(style="whitegrid")

# Creating the plot
plt.figure(figsize=(10, 6))
sns.barplot(x='Importance', y='Feature', data=features_df, palette='viridis')

# Adding title and labels
plt.title('Feature Importances in Gradient Boosting Model')
plt.xlabel('Importance')
plt.ylabel('Features')

# Displaying the plot
plt.show()
