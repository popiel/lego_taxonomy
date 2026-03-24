const {
  COLUMN_IDS,
  CATEGORY_COLUMNS,
  getColumnIndex,
  isCategoryColumn,
  constrainDropTarget,
  moveColumn,
  moveCategoryGroup,
  resetColumns
} = require('../../main/resources/columnOrder');

describe('ColumnOrder', () => {
  describe('COLUMN_IDS', () => {
    it('should have 9 columns in correct order', () => {
      expect(COLUMN_IDS).toHaveLength(9);
      expect(COLUMN_IDS[0]).toBe('category');
      expect(COLUMN_IDS[4]).toBe('image');
      expect(COLUMN_IDS[8]).toBe('partNumber');
    });

    it('should have category columns in first 4 positions', () => {
      expect(COLUMN_IDS.slice(0, 4)).toEqual(['category', 'category2', 'category3', 'category4']);
    });
  });

  describe('CATEGORY_COLUMNS', () => {
    it('should have 4 category columns', () => {
      expect(CATEGORY_COLUMNS).toHaveLength(4);
    });
  });

  describe('getColumnIndex', () => {
    it('should return correct index for each column', () => {
      expect(getColumnIndex(COLUMN_IDS, 'category')).toBe(0);
      expect(getColumnIndex(COLUMN_IDS, 'category2')).toBe(1);
      expect(getColumnIndex(COLUMN_IDS, 'category3')).toBe(2);
      expect(getColumnIndex(COLUMN_IDS, 'category4')).toBe(3);
      expect(getColumnIndex(COLUMN_IDS, 'image')).toBe(4);
      expect(getColumnIndex(COLUMN_IDS, 'color')).toBe(5);
      expect(getColumnIndex(COLUMN_IDS, 'quantity')).toBe(6);
      expect(getColumnIndex(COLUMN_IDS, 'name')).toBe(7);
      expect(getColumnIndex(COLUMN_IDS, 'partNumber')).toBe(8);
    });

    it('should return -1 for non-existent column', () => {
      expect(getColumnIndex(COLUMN_IDS, 'invalid')).toBe(-1);
      expect(getColumnIndex(COLUMN_IDS, 'foo')).toBe(-1);
    });
  });

  describe('isCategoryColumn', () => {
    it('should return true for category columns', () => {
      expect(isCategoryColumn('category')).toBe(true);
      expect(isCategoryColumn('category2')).toBe(true);
      expect(isCategoryColumn('category3')).toBe(true);
      expect(isCategoryColumn('category4')).toBe(true);
    });

    it('should return false for non-category columns', () => {
      expect(isCategoryColumn('image')).toBe(false);
      expect(isCategoryColumn('color')).toBe(false);
      expect(isCategoryColumn('quantity')).toBe(false);
      expect(isCategoryColumn('name')).toBe(false);
      expect(isCategoryColumn('partNumber')).toBe(false);
    });
  });

  describe('constrainDropTarget', () => {
    it('should allow category columns to drop anywhere', () => {
      expect(constrainDropTarget(COLUMN_IDS, 0, true)).toBe(0);
      expect(constrainDropTarget(COLUMN_IDS, 1, true)).toBe(1);
      expect(constrainDropTarget(COLUMN_IDS, 2, true)).toBe(2);
      expect(constrainDropTarget(COLUMN_IDS, 3, true)).toBe(3);
      expect(constrainDropTarget(COLUMN_IDS, 4, true)).toBe(4);
      expect(constrainDropTarget(COLUMN_IDS, 5, true)).toBe(5);
      expect(constrainDropTarget(COLUMN_IDS, 8, true)).toBe(8);
    });

    it('should redirect non-category columns dropped within category range to after category columns', () => {
      expect(constrainDropTarget(COLUMN_IDS, 0, false)).toBe(4);
      expect(constrainDropTarget(COLUMN_IDS, 1, false)).toBe(4);
      expect(constrainDropTarget(COLUMN_IDS, 2, false)).toBe(4);
      expect(constrainDropTarget(COLUMN_IDS, 3, false)).toBe(4);
    });

    it('should allow non-category columns to drop outside category range', () => {
      expect(constrainDropTarget(COLUMN_IDS, 4, false)).toBe(4);
      expect(constrainDropTarget(COLUMN_IDS, 5, false)).toBe(5);
      expect(constrainDropTarget(COLUMN_IDS, 6, false)).toBe(6);
      expect(constrainDropTarget(COLUMN_IDS, 7, false)).toBe(7);
      expect(constrainDropTarget(COLUMN_IDS, 8, false)).toBe(8);
    });
  });

  describe('moveColumn', () => {
    it('should move column from earlier to later position', () => {
      const result = moveColumn(COLUMN_IDS, 0, 8);
      expect(result[0]).toBe('category2');
      expect(result[7]).toBe('category');
      expect(result[8]).toBe('partNumber');
    });

    it('should move column from later to earlier position', () => {
      const result = moveColumn(COLUMN_IDS, 8, 0);
      expect(result[0]).toBe('partNumber');
      expect(result[1]).toBe('category');
      expect(result[8]).toBe('name');
    });

    it('should move column within category range', () => {
      const result = moveColumn(COLUMN_IDS, 0, 3);
      expect(result.slice(0, 4)).toEqual(['category2', 'category3', 'category4', 'category']);
    });

    it('should move non-category column into non-category range', () => {
      const result = moveColumn(COLUMN_IDS, 4, 7);
      expect(result[4]).toBe('color');
      expect(result[6]).toBe('image');
    });

    it('should not modify original array', () => {
      const copy = [...COLUMN_IDS];
      moveColumn(COLUMN_IDS, 0, 8);
      expect(COLUMN_IDS).toEqual(copy);
    });

    it('should handle moving first category column to end of category range', () => {
      const result = moveColumn(COLUMN_IDS, 0, 3);
      expect(result[3]).toBe('category');
    });
  });

  describe('resetColumns', () => {
    it('should return columns to default order', () => {
      const reordered = ['image', 'color', 'category', 'category2', 'category3', 'category4', 'quantity', 'name', 'partNumber'];
      const result = resetColumns();
      expect(result).toEqual(COLUMN_IDS);
      expect(result).not.toBe(COLUMN_IDS);
    });

    it('should return a new array, not the original', () => {
      const result = resetColumns();
      result[0] = 'modified';
      expect(COLUMN_IDS[0]).toBe('category');
    });
  });

  describe('category grouping constraint', () => {
    it('should maintain category columns as group when all are moved', () => {
      const order = ['image', 'category', 'category2', 'category3', 'category4', 'color', 'quantity', 'name', 'partNumber'];
      const result = moveColumn(order, 0, 8);
      const resultCategoryIndices = result.slice(0, 4).filter(col => CATEGORY_COLUMNS.includes(col));
      expect(resultCategoryIndices.length).toBe(4);
    });

    describe('after category repositioning', () => {
      it('should not allow non-category column to insert between category columns', () => {
        const orderAfterCategoryMove = ['image', 'color', 'category', 'category2', 'category3', 'category4', 'quantity', 'name', 'partNumber'];
        const colorIndex = getColumnIndex(orderAfterCategoryMove, 'color');
        expect(colorIndex).toBe(1);
        const category3Index = getColumnIndex(orderAfterCategoryMove, 'category3');
        expect(category3Index).toBe(4);
        const category4Index = getColumnIndex(orderAfterCategoryMove, 'category4');
        expect(category4Index).toBe(5);
        const targetBetweenCategory3And4 = 4;
        const result = constrainDropTarget(orderAfterCategoryMove, targetBetweenCategory3And4, false);
        expect(result).toBeGreaterThan(category4Index);
      });

      it('should not allow non-category column to insert between category columns in default order', () => {
        const nameIndex = getColumnIndex(COLUMN_IDS, 'name');
        expect(nameIndex).toBe(7);
        const result = constrainDropTarget(COLUMN_IDS, 1, false);
        expect(result).toBeGreaterThan(3);
      });

      it('should place name column after category group when dropped between category columns', () => {
        const result = moveColumn(COLUMN_IDS, 7, 1);
        expect(result.slice(0, 4)).toEqual(['category', 'category2', 'category3', 'category4']);
        expect(result[4]).toBe('name');
      });

      it('should keep categories contiguous after repositioning', () => {
        const orderAfterCategoryMove = ['image', 'color', 'category', 'category2', 'category3', 'category4', 'quantity', 'name', 'partNumber'];
        const categoryIndices = orderAfterCategoryMove
          .map((col, idx) => CATEGORY_COLUMNS.includes(col) ? idx : -1)
          .filter(idx => idx >= 0);
        expect(categoryIndices).toEqual([2, 3, 4, 5]);
        expect(categoryIndices[categoryIndices.length - 1] - categoryIndices[0]).toBe(categoryIndices.length - 1);
      });
    });

    describe('moveCategoryGroup', () => {
      it('should move all category columns together when dragging category from default order to after quantity', () => {
        const result = moveCategoryGroup(COLUMN_IDS, 0, 6);
        const categoryColumnsInResult = result.filter(col => CATEGORY_COLUMNS.includes(col));
        expect(categoryColumnsInResult).toHaveLength(4);
      });

      it('should move all category columns together when dragging category to end', () => {
        const result = moveCategoryGroup(COLUMN_IDS, 0, 8);
        expect(result).toEqual(['image', 'color', 'quantity', 'name', 'category', 'category2', 'category3', 'category4', 'partNumber']);
      });

      it('should move all category columns together when dragging category2 to position 5', () => {
        const result = moveCategoryGroup(COLUMN_IDS, 1, 5);
        const categoryColumnsInResult = result.filter(col => CATEGORY_COLUMNS.includes(col));
        expect(categoryColumnsInResult).toHaveLength(4);
      });

      it('should not modify original array', () => {
        const copy = [...COLUMN_IDS];
        moveCategoryGroup(COLUMN_IDS, 0, 6);
        expect(COLUMN_IDS).toEqual(copy);
      });

      it('should not allow moving category column within category range', () => {
        const result = moveCategoryGroup(COLUMN_IDS, 0, 2);
        expect(result).toEqual(COLUMN_IDS);
      });

      it('should not allow moving middle category column within category range', () => {
        const result = moveCategoryGroup(COLUMN_IDS, 1, 3);
        expect(result).toEqual(COLUMN_IDS);
      });

      it('should preserve category column order when moving to end', () => {
        const result = moveCategoryGroup(COLUMN_IDS, 0, 7);
        const categoryOrder = result.filter(col => CATEGORY_COLUMNS.includes(col));
        expect(categoryOrder).toEqual(['category', 'category2', 'category3', 'category4']);
      });

      it('should preserve category column order when moving to middle of non-category', () => {
        const result = moveCategoryGroup(COLUMN_IDS, 0, 5);
        const categoryOrder = result.filter(col => CATEGORY_COLUMNS.includes(col));
        expect(categoryOrder).toEqual(['category', 'category2', 'category3', 'category4']);
      });

      it('should preserve category column order in any valid move', () => {
        const result = moveCategoryGroup(COLUMN_IDS, 2, 7);
        const categoryOrder = result.filter(col => CATEGORY_COLUMNS.includes(col));
        expect(categoryOrder).toEqual(['category', 'category2', 'category3', 'category4']);
      });
    });
  });
});
