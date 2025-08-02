export const validateEmail = (email) => {
  const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/
  return emailRegex.test(email)
}

export const validatePassword = (password) => {
  if (password.length < 8) {
    return 'Password must be at least 8 characters long'
  }
  if (!/[A-Z]/.test(password)) {
    return 'Password must contain at least one uppercase letter'
  }
  if (!/[a-z]/.test(password)) {
    return 'Password must contain at least one lowercase letter'
  }
  if (!/[0-9]/.test(password)) {
    return 'Password must contain at least one number'
  }
  if (!/[!@#$%^&*()_+\-=[\]{};':"\\|,.<>/?]/.test(password)) {
    return 'Password must contain at least one special character'
  }
  return true
}

export const validateUsername = (username) => {
  if (!username) {
    return 'Username is required'
  }
  if (username.length < 3) {
    return 'Username must be at least 3 characters long'
  }
  if (username.length > 20) {
    return 'Username must be no more than 20 characters long'
  }
  if (!/^[a-zA-Z0-9_-]+$/.test(username)) {
    return 'Username can only contain letters, numbers, underscores, and hyphens'
  }
  return true
}

export const validateSignupForm = (formData) => {
  const errors = {}
  
  const usernameValidation = validateUsername(formData.username)
  if (usernameValidation !== true) {
    errors.username = usernameValidation
  }
  
  if (!validateEmail(formData.email)) {
    errors.email = 'Please enter a valid email address'
  }
  
  const passwordValidation = validatePassword(formData.password)
  if (passwordValidation !== true) {
    errors.password = passwordValidation
  }
  
  if (formData.password !== formData.confirmPassword) {
    errors.confirmPassword = 'Passwords do not match'
  }
  
  return errors
}