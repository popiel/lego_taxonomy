(function (root, factory) {
  if (typeof module === 'object' && module.exports) {
    module.exports = factory();
  } else {
    root.columnOrder = factory();
  }
}(typeof self !== 'undefined' ? self : this, function () {
  const COLUMN_IDS = ['category', 'category2', 'category3', 'category4', 'image', 'color', 'quantity', 'name', 'partNumber'];
  const CATEGORY_COLUMNS = ['category', 'category2', 'category3', 'category4'];

  function getColumnIndex(order, columnId) {
    return order.indexOf(columnId);
  }

  function isCategoryColumn(columnId) {
    return CATEGORY_COLUMNS.includes(columnId);
  }

  function getCategoryRange(order) {
    const indices = order
      .map((col, idx) => CATEGORY_COLUMNS.includes(col) ? idx : -1)
      .filter(idx => idx >= 0);
    if (indices.length === 0) return { start: -1, end: -1 };
    return { start: indices[0], end: indices[indices.length - 1] };
  }

  function constrainDropTarget(order, targetIndex, isDraggingCategory) {
    const range = getCategoryRange(order);
    if (range.start === -1) return targetIndex;
    if (isDraggingCategory) {
      return targetIndex;
    }
    if (targetIndex >= range.start && targetIndex <= range.end) {
      return range.end + 1;
    }
    return targetIndex;
  }

  function moveColumn(order, fromIndex, toIndex) {
    const range = getCategoryRange(order);
    const newOrder = [...order];
    const [moved] = newOrder.splice(fromIndex, 1);
    const minNonCategoryIndex = range.end + 1;
    const insertIndex = toIndex > fromIndex && toIndex > minNonCategoryIndex ? toIndex - 1 : toIndex;
    newOrder.splice(insertIndex, 0, moved);
    return newOrder;
  }

  function moveCategoryGroup(order, fromIndex, toIndex) {
    const newOrder = [...order];
    const categoryColumnsInOrder = newOrder.filter(col => CATEGORY_COLUMNS.includes(col));
    if (categoryColumnsInOrder.length === 0) return newOrder;
    
    const range = getCategoryRange(order);
    const isDraggingFromCategoryRange = fromIndex <= range.end;
    
    if (!isDraggingFromCategoryRange) {
      return moveColumn(order, fromIndex, toIndex);
    }
    
    const targetIsInCategoryRange = toIndex <= range.end;
    if (targetIsInCategoryRange) {
      return [...order];
    }
    
    const categoryCount = categoryColumnsInOrder.length;
    let adjustedToIndex = toIndex;
    
    if (toIndex > fromIndex) {
      adjustedToIndex = toIndex - categoryCount;
    } else {
      adjustedToIndex = Math.max(toIndex - categoryCount + 1, range.end + 1);
    }
    
    const removedCategories = newOrder.splice(range.start, categoryCount);
    newOrder.splice(adjustedToIndex, 0, ...removedCategories);
    
    return newOrder;
  }

  function resetColumns() {
    return [...COLUMN_IDS];
  }

  return {
    COLUMN_IDS,
    CATEGORY_COLUMNS,
    getColumnIndex,
    isCategoryColumn,
    getCategoryRange,
    constrainDropTarget,
    moveColumn,
    moveCategoryGroup,
    resetColumns
  };
}));
