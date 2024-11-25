from transformers import pipeline

# Cargar el modelo preentrenado para análisis de sentimientos
#sentiment_classifier = pipeline('sentiment-analysis')

# # Ejemplos de texto para clasificar
# textos = ["Estoy muy contento con este servicio",
#           "No me gusta para nada este producto",
#           "Me siento indiferente hacia este tema"]
# 
# # Clasificar emociones
# for texto in textos:
#     resultado = sentiment_classifier(texto)
#     print(f"Texto: {texto}\nEmoción: {resultado[0]['label']}, Confianza: {resultado[0]['score']:.4f}\n")


# Cargar el modelo preentrenado para análisis de emociones en español
emotion_classifier = pipeline('sentiment-analysis', model='finiteautomata/beto-emotion-analysis', tokenizer='finiteautomata/beto-emotion-analysis')

