import fetch from 'node-fetch';

// Direct backend API URL (not proxied)
const API_BASE_URL = 'http://localhost:8080/api/v1';

// Store created test users for cleanup
const createdUsers = new Set();

export const testUserHelper = {
  /**
   * Create a test user via the API
   * @param {string} email - User's email
   * @param {string} password - User's password
   * @returns {Promise<Object>} Created user data
   */
  async createTestUser(email, password) {
    try {
      // Generate username from email (matching the logic in SignupForm)
      const emailParts = email.split('@');
      let username = emailParts[0]
        .toLowerCase()
        .replace(/[^a-z0-9_-]/g, '')
        .substring(0, 30);
      
      if (username.length < 3) {
        username = 'user' + Date.now().toString().slice(-6);
      }

      const userData = {
        username,
        email,
        password
      };

      console.log(`Creating test user: ${email} with username: ${username}`);

      const response = await fetch(`${API_BASE_URL}/auth/register`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(userData)
      });

      if (!response.ok) {
        const error = await response.text();
        throw new Error(`Failed to create test user: ${response.status} - ${error}`);
      }

      const user = await response.json();
      
      // Store for cleanup
      createdUsers.add({
        email,
        username,
        id: user.id
      });

      console.log(`Test user created successfully: ${email}`);
      return user;
    } catch (error) {
      console.error('Error creating test user:', error);
      throw error;
    }
  },

  /**
   * Delete a test user
   * @param {string} email - User's email to delete
   */
  async deleteTestUser(email) {
    try {
      // First, we need to login as admin or use a delete endpoint
      // For now, we'll track that we need to implement this
      console.log(`TODO: Delete test user: ${email}`);
      
      // Remove from tracking
      for (const user of createdUsers) {
        if (user.email === email) {
          createdUsers.delete(user);
          break;
        }
      }
    } catch (error) {
      console.error('Error deleting test user:', error);
    }
  },

  /**
   * Clean up all created test users
   */
  async cleanupAllTestUsers() {
    console.log(`Cleaning up ${createdUsers.size} test users...`);
    
    const users = Array.from(createdUsers);
    for (const user of users) {
      await this.deleteTestUser(user.email);
    }
    
    createdUsers.clear();
  },

  /**
   * Check if a user exists (for negative test cases)
   * @param {string} email 
   * @returns {Promise<boolean>}
   */
  async userExists(email) {
    try {
      // Try to login with the email - if it fails with "user not found" then user doesn't exist
      const response = await fetch(`${API_BASE_URL}/auth/login`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          email,
          password: 'dummy' // We just want to check if user exists
        })
      });

      // If we get 401, user might exist but password is wrong
      // If we get 404 or specific error, user doesn't exist
      const responseText = await response.text();
      return !responseText.includes('not found') && response.status !== 404;
    } catch (error) {
      return false;
    }
  }
};

export default testUserHelper;