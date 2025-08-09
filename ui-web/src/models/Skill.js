export default class Skill {
  name = ''
  attribute = 'Agility'
  description = ''

  constructor(data = {}) {
    this.name = data.name || '';
    this.attribute = data.attribute || 'Agility';
    this.description = data.description || '';
  }

  isValidAttribute(attribute) {
    const validAttributes = ['Agility', 'Smarts', 'Spirit', 'Strength', 'Vigor'];
    return validAttributes.includes(attribute);
  }

  updateAttribute(newAttribute) {
    if (this.isValidAttribute(newAttribute)) {
      this.attribute = newAttribute;
    }
  }

  clone() {
    return new Skill({
      name: this.name,
      attribute: this.attribute,
      description: this.description
    });
  }

  toJSON() {
    return {
      name: this.name,
      attribute: this.attribute,
      description: this.description
    };
  }
}