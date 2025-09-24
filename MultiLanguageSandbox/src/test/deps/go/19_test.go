package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func answerQuestions() string {
    return "CCDCA"
}

func TestAnswerQuestions(t *testing.T) {
	expectedAnswers := "DDDBA"
	actualAnswers := answerQuestions()

	assert.Equal(t, expectedAnswers, actualAnswers, "they should be equal")
}