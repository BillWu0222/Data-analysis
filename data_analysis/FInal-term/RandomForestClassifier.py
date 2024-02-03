import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score, classification_report, confusion_matrix
from sklearn.metrics import classification_report

# Load the dataset
data= pd.read_csv('Win_Lose_Data.csv')
data

# Convert 'status' column to binary (win = 1, lose and draw = 0)
data['status'] = data['status'].apply(lambda x: 1 if x == 'win' else 0)

data.tail()

# Select the zodiac sign columns (F to U) and the win rate column (E)
zodiac_columns = ['Aries', 'Leo', 'Sagittarius', 'Cancer', 'Scopio', 'Pisces', 'Taurus', 'Virgo', 'Capricorn', 'Gemini', 'Libra', 'Aquarius']
target_column = 'status'

# Calculate correlation matrix
correlation_matrix = data[zodiac_columns + [target_column]].corr()

# Plotting the correlation matrix
plt.figure(figsize=(12, 8))
sns.heatmap(correlation_matrix, annot=True, cmap='coolwarm')
plt.title('Correlation between Zodiac Signs and Win Rate')
plt.show()

# Splitting the dataset into features (X) and target (Y)
X = data[zodiac_columns]
Y = data[target_column]

# Splitting the data with stratification to ensure class distribution is similar in both sets
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.2, stratify=Y, random_state=42)

# Checking the distribution in the training and testing sets
train_distribution = Y_train.value_counts()
test_distribution = Y_test.value_counts()

print("Class distribution in training set:\n", train_distribution)
print("Class distribution in testing set:\n", test_distribution)

# Training the model using RandomForestClassifier
random_forest_model = RandomForestClassifier(n_estimators=100, random_state=42)
random_forest_model.fit(X_train, Y_train)

# Prediction and Evaluation
X_test_prediction = random_forest_model.predict(X_test)
report = classification_report(Y_test, X_test_prediction, output_dict=True)

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

# accuracy on test data
X_test_prediction = random_forest_model.predict(X_test)
test_data_accuracy = accuracy_score(X_test_prediction, Y_test)
print('Accuracy : ',test_data_accuracy)

# Define the input data: counts of each zodiac sign in a team
input_data = (1, 1, 1, 1, 0, 0, 0, 1, 1, 3, 1, 1)

# Convert the input data to a NumPy array
input_data_as_numpy_array = np.asarray(input_data)

# Reshape the data for predicting the label for only one instance
# The model expects the input to be two-dimensional, hence the reshape
input_data_reshaped = input_data_as_numpy_array.reshape(1, -1)

# Use the model to make a prediction
# The predict method will return an array of predictions
prediction = random_forest_model.predict(input_data_reshaped)
print(prediction)

# Output 'Win' or 'Lose' based on the prediction
# prediction[0] is the predicted label for the first (and only) instance
if prediction[0] == 1:
    print('Win')
else:
    print('Lose')
    

from sklearn.metrics import roc_curve, auc
import matplotlib.pyplot as plt

Y_pred_proba = random_forest_model.predict_proba(X_test)[:, 1]

# Compute ROC curve and AUC
fpr, tpr, thresholds = roc_curve(Y_test, Y_pred_proba)
roc_auc = auc(fpr, tpr)

# Plotting
plt.figure(figsize=(8, 6))
plt.plot(fpr, tpr, color='black', lw=2)  # Change the line color to black
plt.fill_between(fpr, tpr, step='post', alpha=0.2, color='grey')  # Fill the area under the curve
plt.plot([0, 1], [0, 1], color='darkgray', lw=2, linestyle='--')  # Diagonal dashed line
plt.xlabel('1 - Specificity')
plt.ylabel('Sensitivity')
plt.title('ROC Curve')

# Display AUC on the plot
plt.text(0.05, 0.95, f'AUC = {roc_auc:.2f}', fontsize=12, va='bottom', ha='left')
plt.show()

# Extracting feature importances
feature_importances = random_forest_model.feature_importances_

# Creating a DataFrame for easier visualization
features_df = pd.DataFrame({'Feature': X_train.columns, 'Importance': feature_importances})
features_df = features_df.sort_values(by='Importance', ascending=False)

# Displaying the most important features
print(features_df)


import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Sample feature importances and feature names from the user's model output
feature_importances = [0.098937, 0.093613, 0.090732, 0.090329, 0.089578, 0.088891, 0.085442,
                       0.083939, 0.078068, 0.070495, 0.068372, 0.061603]
features = ['Pisces', 'Aries', 'Virgo', 'Taurus', 'Aquarius', 'Gemini', 'Capricorn', 
            'Leo', 'Cancer', 'Scorpio', 'Sagittarius', 'Libra']

# Creating a DataFrame for feature importances
features_df = pd.DataFrame({
    'Feature': features, 
    'Importance': feature_importances
})

# Sorting the DataFrame by importance
features_df = features_df.sort_values(by='Importance', ascending=False)

# Plotting the feature importances
plt.figure(figsize=(8, 4))
sns.barplot(x='Importance', y='Feature', data=features_df, palette='viridis')
plt.title('Feature Importances in Random Forest Model')
plt.xlabel('Importance')
plt.ylabel('Feature')
plt.show()


