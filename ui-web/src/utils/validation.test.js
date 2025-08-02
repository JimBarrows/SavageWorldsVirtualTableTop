import { 
  validateEmail, 
  validatePassword, 
  validateUsername,
  validateSignupForm 
} from './validation'

describe('validation utilities', () => {
  describe('validateEmail', () => {
    it('returns true for valid email addresses', () => {
      expect(validateEmail('test@example.com')).toBe(true)
      expect(validateEmail('user.name@domain.co.uk')).toBe(true)
      expect(validateEmail('user+tag@example.org')).toBe(true)
      expect(validateEmail('user123@sub.domain.com')).toBe(true)
    })

    it('returns false for invalid email addresses', () => {
      expect(validateEmail('invalid-email')).toBe(false)
      expect(validateEmail('@example.com')).toBe(false)
      expect(validateEmail('user@')).toBe(false)
      expect(validateEmail('user @example.com')).toBe(false)
      expect(validateEmail('user@example')).toBe(false)
      expect(validateEmail('')).toBe(false)
    })
  })

  describe('validatePassword', () => {
    it('returns true for valid passwords', () => {
      expect(validatePassword('ValidPass123!')).toBe(true)
      expect(validatePassword('Str0ng@Password')).toBe(true)
      expect(validatePassword('P@ssw0rd123')).toBe(true)
      expect(validatePassword('Complex1ty!')).toBe(true)
    })

    it('returns error for passwords less than 8 characters', () => {
      expect(validatePassword('Short1!')).toBe('Password must be at least 8 characters long')
    })

    it('returns error for passwords without uppercase letter', () => {
      expect(validatePassword('lowercase123!')).toBe('Password must contain at least one uppercase letter')
    })

    it('returns error for passwords without lowercase letter', () => {
      expect(validatePassword('UPPERCASE123!')).toBe('Password must contain at least one lowercase letter')
    })

    it('returns error for passwords without number', () => {
      expect(validatePassword('NoNumbers!')).toBe('Password must contain at least one number')
    })

    it('returns error for passwords without special character', () => {
      expect(validatePassword('NoSpecial123')).toBe('Password must contain at least one special character')
    })
  })

  describe('validateUsername', () => {
    it('returns true for valid usernames', () => {
      expect(validateUsername('testuser')).toBe(true)
      expect(validateUsername('user123')).toBe(true)
      expect(validateUsername('test_user')).toBe(true)
      expect(validateUsername('user-name')).toBe(true)
    })

    it('returns error for usernames less than 3 characters', () => {
      expect(validateUsername('ab')).toBe('Username must be at least 3 characters long')
    })

    it('returns error for usernames more than 20 characters', () => {
      expect(validateUsername('verylongusernamethatexceedsthelimit')).toBe('Username must be no more than 20 characters long')
    })

    it('returns error for usernames with invalid characters', () => {
      expect(validateUsername('user@name')).toBe('Username can only contain letters, numbers, underscores, and hyphens')
      expect(validateUsername('user name')).toBe('Username can only contain letters, numbers, underscores, and hyphens')
      expect(validateUsername('user.name')).toBe('Username can only contain letters, numbers, underscores, and hyphens')
    })

    it('returns error for empty username', () => {
      expect(validateUsername('')).toBe('Username is required')
    })
  })

  describe('validateSignupForm', () => {
    it('returns empty object for valid form data', () => {
      const formData = {
        username: 'testuser',
        email: 'test@example.com',
        password: 'ValidPass123!',
        confirmPassword: 'ValidPass123!'
      }
      expect(validateSignupForm(formData)).toEqual({})
    })

    it('returns username error when username is invalid', () => {
      const formData = {
        username: 'ab',
        email: 'test@example.com',
        password: 'ValidPass123!',
        confirmPassword: 'ValidPass123!'
      }
      const errors = validateSignupForm(formData)
      expect(errors.username).toBe('Username must be at least 3 characters long')
    })

    it('returns email error when email is invalid', () => {
      const formData = {
        username: 'testuser',
        email: 'invalid-email',
        password: 'ValidPass123!',
        confirmPassword: 'ValidPass123!'
      }
      const errors = validateSignupForm(formData)
      expect(errors.email).toBe('Please enter a valid email address')
    })

    it('returns password error when password is invalid', () => {
      const formData = {
        username: 'testuser',
        email: 'test@example.com',
        password: 'weak',
        confirmPassword: 'weak'
      }
      const errors = validateSignupForm(formData)
      expect(errors.password).toBe('Password must be at least 8 characters long')
    })

    it('returns confirmPassword error when passwords do not match', () => {
      const formData = {
        username: 'testuser',
        email: 'test@example.com',
        password: 'ValidPass123!',
        confirmPassword: 'DifferentPass123!'
      }
      const errors = validateSignupForm(formData)
      expect(errors.confirmPassword).toBe('Passwords do not match')
    })

    it('returns multiple errors when multiple fields are invalid', () => {
      const formData = {
        username: '',
        email: 'invalid',
        password: 'weak',
        confirmPassword: 'different'
      }
      const errors = validateSignupForm(formData)
      expect(Object.keys(errors).length).toBe(4)
      expect(errors.username).toBeDefined()
      expect(errors.email).toBeDefined()
      expect(errors.password).toBeDefined()
      expect(errors.confirmPassword).toBeDefined()
    })
  })
})